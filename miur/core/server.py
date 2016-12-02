import logging
import asyncio
import threading
# import functools

from . import bus
from miur.share import protocol

_log = logging.getLogger(__name__)


# ALT: replace by coro handler
#   http://stackoverflow.com/questions/37452039/how-to-correct-asyncio-yield-from-in-data-received
class ClientProtocol(asyncio.Protocol):
    _clients = {}
    _lock = threading.Lock()

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))
        self.cid = self.peer  # ALT: use first msg from client as its cid
        with ClientProtocol._lock:
            ClientProtocol._clients[self.cid] = self

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        with ClientProtocol._lock:
            # BAD: crash if waits on _lock until disconnectAll() exits
            del ClientProtocol._clients[self.cid]

    def data_received(self, data):
        obj, ifmt = protocol.deserialize(data)
        _log.info('Recv:{!r}: {!r}'.format(self.cid, obj))
        bus.qin.put_nowait((self.cid, (ifmt, obj)))

    def send_data(self, obj, ofmt=None):
        _log.info('Send:{!r}: {!r}'.format(self.cid, obj))
        data = protocol.serialize(obj, ofmt)
        # BAD: exc if client was already deleted when executor was suspended
        self.transport.write(data)

    @staticmethod
    async def send(cid, obj, ofmt=None):
        # NOTE: can't await send_data w/o create_task
        with ClientProtocol._lock:
            return ClientProtocol._clients[cid].send_data(obj, ofmt)

    @staticmethod
    def disconnectAll():
        # BAD: won't close any connection, waiting on _lock to add
        with ClientProtocol._lock:
            for client in ClientProtocol._clients.values():
                _log.info('Closing the client {!r} socket'.format(client))
                client.transport.close()
