# Multidimensional Scaling Server for Lisp

This is a client server arrangement for calling Python multidimensional scaling code from Lisp.
It consists of two pieces, a server written in Python, and a client written in Common Lisp.
By default they communicate using port 9892.


## Using the client

With QuickLisp installed, load the file `multidimensional-scaling.lisp.`

Then call the Lisp function `mds:scale`. This function takes one required argument, a list of lists of real numbers representing a square matrix, in row major order.
It also takes sseveral optional keyword arguments:

* `:initializations`, if supplied should be a positive integer, which is the number of distinct runs with unique random states that are made, the best result being returned;
  it defaults to `4`

* `:maximum-iterations`, if supplied should be a positive integer, which is the maximum number of iterations of each run used to try to converge; it defaults to `300`

* `:tolgerance`, if supplied should be a small, positive float, which is used to decide whether or not the iterative MDS process has converged; it defaults to `0.001`

* `:host`, if supplied should be a string, the host name or IP address, the location of the Python server; it defaults to `koalemos.psy.cmu.edu`

* `:port`, if supplied it should be a positive integer, the port to use for communicating with the server; it defaults to `9892`.

The required matrix argument should be symmetrical, or at least close to. If it is not symmetrical it is forced into that form by replacing both
elements of each of the  corresponding pairs of elements on opposite sides of the main diagonal by their means. This is sensible if the values are
reasonably close, perhaps only differing because of round off errors. If they are radically different the results may make no sense.
The values of the elements of this matrix should be ACT-R similarity values in the range [-1, 0]. If, after any necessary averaging of pairs,
they are outside this range then, after conversion of [0, 1] dissimilarities they are silently replaced by 0 or 1 as appropriate.

The return value is a list of two element lists, each sublist representing one point in the reduced space.

For example, calling

    (mds:scale '((0.000 -0.406 -1.000 10.000)
                (-0.408 0.000 -1.000 -1.000)
                (-1.000 -1.000 0.000 -0.407)
                (-10.000 -1.000 -0.407 0.000)))

returns a value something like

    ((-0.0714357039973051d0 0.21229560989666427d0)
     (0.4266129567719454d0 0.39484829729621795d0)
     (-0.13511412763785902d0 -0.5653741809915379d0)
     (-0.2200631251367813d0 -0.04176972620134434d0))

The actual numbers returned will vary with each call since the MDS process is stochastic.

There is little error checking or handling. If bad arguments are passed or there is some other error tears may result.
Given the use we expect to make of this it does not seem worth investing in armor plating.

This code has been tested, though only cursorily, in SBCL and CCL on Linux.
Sadly I currently am unable to run Lisp on a Macintosh. I *hope* this will work just fine there, too.
It may be worth noting that for small examples such as the above it is a little
over 100 times faster in SBCL than CCL. It seems likely that the ratio will decrease
substantially for larger cases where the bulk of the time is spent in the Python
server instead of in the Lisp client code.


## Running the server

First create a virtual environment, activate it, and do `pip install -r requirements.txt`  in it.

Then simply run `python multidimensional-scaling-server.py` in it. You likely will want
to run it in the background, and under `nohup`.

To change the port used pass the `--port` (or `-p`) argument when starting the server.

The server has been cursorily tested with Python 3.10 in Ubuntu Linux 22.04 LTS, but probably will work with several earlier versions of Python 3, as well as later versions,
and in other forms of Unix. Maybe it’ll work in Windows, too, who knows?

Again there is very little error handling or argument checking, though it does try its best to stay up, writing error messages to standard out.

As of the time of writing there is an instance of this server running on Koalemos.


## License

> Copyright (c) 2024 Carnegie Mellon University
>
> Permission is hereby granted, free of charge, to any person obtaining a copy of this
> software and associated documentation files (the "Software"), to deal in the Software
> without restriction, including without limitation the rights to use, copy, modify,
> merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to the following
> conditions:
>
> The above copyright notice and this permission notice shall be included in all copies
> or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
> INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
> PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
> HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
> CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
> OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
