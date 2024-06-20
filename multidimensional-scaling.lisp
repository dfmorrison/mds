;;; Copyright (c) 2024 Carnegie Mellon University
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(ql:quickload '(:alexandria :usocket :com.inuoe.jzon))

(defpackage :multidimensional-scaling
  (:nicknames :mds)
  (:use :common-lisp :alexandria :usocket)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export #:scale))

(in-package :multidimensional-scaling)

(defparameter +default-host+ "koalemos.psy.cmu.edu")
(defparameter +default-port+ 9892)

(defun square-matrix-p (list)
  (and (listp list)
       (let ((dimension (length list)))
         (every (lambda (sublist)
                  (and (listp sublist)
                       (eql (length sublist) dimension)
                       (every #'realp sublist)))
                list))))

(defun symmetricize (square-array &key (minimum most-negative-long-float)
                                    (maximum most-positive-long-float))
  ;; typically will signal an error if argument is not a square array of reals
  (let ((dimension (array-dimension square-array 0)))
    (dotimes (i dimension)
      (setf (aref square-array i i) (clamp (aref square-array i i) minimum maximum))
      (unless (zerop i)
        (dotimes (j i)
          (let ((mean (clamp (/ (+ (aref square-array i j) (aref square-array j i)) 2.0)
                             minimum
                             maximum)))
            (setf (aref square-array i j) mean)
            (setf (aref square-array j i) mean))))))
  square-array)

(defun scale (similarities &key (initializations 4)
                             (maximum-iterations 300)
                             (tolerance 1.0e-3)
                             (host +default-host+)
                             (port +default-port+))
  (unless (square-matrix-p similarities)
    (error "~S does not appear to be a square matrix of reals represented as a list of lists"
           similarities))
  (let* ((dimension (length similarities))
         (matrix (make-array (list dimension dimension) :initial-contents similarities))
         (message (plist-hash-table `(matrix ,matrix
                                      n-init ,initializations
                                      max-iter ,maximum-iterations
                                      eps ,tolerance))))
    ;; convert ACT-R similarities to [0, 1] dissimilarities
    (dotimes (i dimension)
      (dotimes (j dimension)
        (unless (zerop (aref matrix i j))
          (setf (aref matrix i j) (- (aref matrix i j))))))
    (symmetricize matrix :minimum 0 :maximum 1)
    (let ((socket (socket-connect host port)))
      (unwind-protect
           (let ((stream (socket-stream socket)))
             (format stream "~A~%" (jzon:stringify message))
             (finish-output stream)
             (map 'list (rcurry #'coerce 'list) (jzon:parse (read-line stream))))
        (socket-close socket)))))
