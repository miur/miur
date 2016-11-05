import pickle
import socket


def send(server_address, message):
    if isinstance(server_address, tuple):
        family = socket.AF_INET
    else:
        family = socket.AF_UNIX

    data = pickle.dumps(message, protocol=pickle.HIGHEST_PROTOCOL)

    # Create a socket (SOCK_STREAM means a TCP socket)
    with socket.socket(family, socket.SOCK_STREAM) as sock:
        sock.connect(server_address)

        sock.sendall(data)
        # sock.sendfile(f)  # ret nbytes
        # sock.sendall(bytes(message, 'ascii'))

        try:
            # FIXME: magic 1024 -- how process packets without limitations?
            response = pickle.loads(sock.recv(1024))
        # BAD: unreliable and slow
        except (pickle.UnpicklingError, KeyError, EOFError):
            response = data.decode()
        # response = str(sock.recv(1024), 'ascii')

        # FIXME: print to bkgr screen, don't overlap with ncurses window
        # print("Received: {}".format(response))

        # THINK: when to close 'sock' -- after each request? Or after session?
        return response
