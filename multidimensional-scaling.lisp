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

(ql:quickload '(#:alexandria #:usocket #:com.inuoe.jzon))

(defpackage #:multidimensional-scaling
  (:nicknames #:mds)
  (:use #:common-lisp #:alexandria #:usocket)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)))

(in-package #:multidimensional-scaling)

(defparameter +default-host+ "koalemos.psy.cmu.edu")
(defparameter +default-port+ 9892)

(defun make-request ()
  ;; TODO stub
  (alist-hash-table `((:word . ,(format nil "~R" (1+ (random 20))))
                      (:repeat . ,(1+ (random 10))))))

(defun run (&key (host +default-host+) (port +default-port+))
  ;; TODO stub
  (let ((socket (usocket:socket-connect host port)))
    (:_ socket)
    (unwind-protect
         (let ((stream (usocket:socket-stream socket))
               (data (make-request)))
           (:_ stream data0
           (format t ";; request: ~S~%" data)
           (setf data (jzon:stringify data))
           (format t ";; sending: ~S~%" data)
           (format stream "~A~%" data)
           (finish-output stream)
           (setf data (read-line stream))
           (format t ";; received: ~S~%" data)
           (format t ";; decoded response: ~S~%" (jzon:parse data)))
      (usocket:socket-close socket))))
