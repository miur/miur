import logging
import asyncio
# import functools

from miur.share import protocol
# BAD: don't like this circular deps
from . import entrepot
from .client import ClientsList

_log = logging.getLogger(__name__)


# ALT: replace by coro handler
#   http://stackoverflow.com/questions/37452039/how-to-correct-asyncio-yield-from-in-data-received
class ClientProtocol(asyncio.Protocol):
    _clients = ClientsList()

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))
        self.cid = self.peer  # ALT: use first msg from client as its cid
        ClientProtocol._clients[self.cid] = self

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        del ClientProtocol._clients[self.cid]

    def data_received(self, data):
        obj, ifmt = protocol.deserialize(data)
        _log.info('Recv:{!r}: {!r}'.format(self.cid, obj))
        entrepot.put_in((self.cid, (ifmt, obj)))

    def send_data(self, obj, ofmt=None):
        _log.info('Send:{!r}: {!r}'.format(self.cid, obj))
        data = protocol.serialize(obj, ofmt)
        # BAD: exc if client was already deleted when executor was suspended
        self.transport.write(data)

    @staticmethod
    async def send(cid, obj, ofmt=None):
        # NOTE: can't await send_data w/o create_task
        return ClientProtocol._clients[cid].send_data(obj, ofmt)


class Bay:
    def __init__(self, server_address, loop):
        self.server_address = server_address
        self.loop = loop

    def __enter__(self):
        # Each client connection will create a new protocol instance
        # functools.partial(ClientProtocol, loop, callback),
        coro = self.loop.create_server(ClientProtocol, *self.server_address,
                                       reuse_address=True, reuse_port=True)
        self.server = self.loop.run_until_complete(coro)
        _log.info('Serving on {}'.format(self.server.sockets[0].getsockname()))

    def __exit__(self, *args):
        ClientProtocol._clients.disconnectAll()
        self.server.close()
        self.loop.run_until_complete(self.server.wait_closed())
