# import io
import pickle
import threading
import socketserver

# MOVE: to global dispatcher
from miur import dom


class ThreadedTCPRequestHandler(socketserver.BaseRequestHandler):
    def handle(self):
        data = pickle.loads(self.request.recv(1024))
        # data = str(self.request.recv(1024), 'ascii')
        cur_thread = threading.current_thread()

        dom.update(data)

        response = bytes("{}: {}".format(cur_thread.name, 'done'), 'ascii')
        self.request.sendall(response)


# NOTE: useful for cli comm ? Works like pipe -- no need to wait on EOF
class ThreadedTCPStreamHandler(socketserver.StreamRequestHandler):
    def handle(self):
        # rfile, wfile -- are file-like objects created by the handler
        # we can now use e.g. readline() instead of raw recv() calls
        self.data = self.rfile.readline().strip()
        print("{} wrote:".format(self.client_address[0]))
        print(self.data)
        # write back to the client
        self.wfile.write(self.data.upper())


class Listener:
    def __init__(self, server_address):
        if isinstance(server_address, tuple):
            # Port 0 means to select an arbitrary unused port
            make = socketserver.ThreadingTCPServer
        else:
            make = socketserver.UnixStreamServer
        self.server = make(server_address, ThreadedTCPRequestHandler)

    def __enter__(self):
        # Start a thread with the server -- that thread will then start one
        # more thread for each request
        server_thread = threading.Thread(target=self.server.serve_forever)
        # Exit the server thread when the main thread terminates
        server_thread.daemon = True
        server_thread.start()
        print("Server loop running in thread:", server_thread.name)
        return self

    def __exit__(self, *args):
        self.server.shutdown()
        self.server.server_close()
