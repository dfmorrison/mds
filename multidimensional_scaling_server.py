# Copyright (c) 2024 Carnegie Mellon University
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this
# software and associated documentation files (the "Software"), to deal in the Software
# without restriction, including without limitation the rights to use, copy, modify,
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to the following
# conditions:
#
# The above copyright notice and this permission notice shall be included in all copies
# or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import json
import numpy as np
from sklearn.manifold import MDS
import socketserver

HOST = "0.0.0.0"
PORT = 9892


class ExampleHandler (socketserver.StreamRequestHandler):

    def handle(self):
        line = self.rfile.readline()
        try:
            data = json.loads(line.decode("utf-8").strip())
            mds = MDS(dissimilarity="precomputed",
                      n_init=data.get("n-init", 4),
                      max_iter=data.get("max-iter", 300),
                      eps=data.get("eps", 1e-3))
            data = json.dumps(mds.fit_transform(data["matrix"]).tolist()) + "\n"
            self.wfile.write(bytes(data, "utf-8"))
            self.wfile.flush()
        except Exception as e:
            print(e)


def main():
    with socketserver.TCPServer((HOST, PORT), ExampleHandler) as server:
        while True:
            server.handle_request()

if __name__== "__main__":
    main()
