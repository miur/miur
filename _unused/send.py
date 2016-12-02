import time
import socket
import logging

from miur.share import protocol

_log = logging.getLogger(__name__)


def send_once(server_address, obj):
    if isinstance(server_address, tuple):
        family = socket.AF_INET
    else:
        family = socket.AF_UNIX

    with socket.socket(family, socket.SOCK_STREAM) as sock:
        sock.connect(server_address)

        data = protocol.serialize(obj)
        sock.sendall(data)  # ALT: sock.sendfile(f)  # ret nbytes
        _log.info("Sent: {}".format(obj))

        # FIXME: magic 1024 -- how process packets without limitations?
        data = sock.recv(1024)
        obj, _ = protocol.deserialize(data)
        _log.info("Recv: {}".format(obj))

        return obj


# USAGE: prs.append(mp.Process(target=test, args=(server_address,'test'))
def test(server_address, obj):
    try:
        time.sleep(0.5)
        send_once(server_address, obj)
    except KeyboardInterrupt:
        pass
