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
        response = str(sock.recv(1024), 'ascii')
        print("Received: {}".format(response))
