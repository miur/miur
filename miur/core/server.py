import logging
import asyncio
import threading

from . import bus
from miur.share import protocol

_log = logging.getLogger(__name__)


# NOTE: mixing ClientProtocol with static vars is BAD idea
# BETTER: re-impl loop.create_server(...) for deterministic order of accept/receive/lost
#   => then parallelism will be controllable and broken lock may become unnecessary
# NOTE: list of connections must be sep entity
#   << if you create two separate servers -- one for TCP and another for UDP
#       -- to support two diff clients lists
class ClientConnections(dict):
    def __init__(self):
        self._lock = threading.Lock()

    def __getitem__(self, cid):
        # THINK? items have refcount so calling their methods is threadsafe after getitem?
        with self._lock:
            return super().__getitem__(cid)

    def __setitem__(self, cid, obj):
        with self._lock:
            return super().__setitem__(cid, obj)

    def __delitem__(self, cid):
        # BAD: crash if waits on _lock until disconnectAll() exits
        with self._lock:
            _log.info('Client {} removed'.format(cid))
            return super().__delitem__(cid)

    # ALT: overload generator :: [c.close() for c in conn.values()]
    def disconnectAll(self):
        # BAD: won't close any connection pending on _lock to in __setitem__
        with self._lock:
            for cid, obj in self.items():
                _log.info('Closing the client {!r}'.format(cid))
                obj.close()


class ClientProtocol(asyncio.Protocol):
    """One instance of this class is created for each connected client"""

    def __init__(self, loop, conn):
        self.loop = loop
        self.conn = conn
        self.is_processing = True

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))
        # ALT: use first msg from client as its cid
        # NOTE: adding to dict don't require lock (beside iterating that dict)
        self.conn[self.peer] = self

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        del self.conn[self.peer]

    def close(self):
        _log.info('Closing the client {!r}'.format(self.peer))
        self.transport.close()

    def data_received(self, data):
        # NEED:DEV: loop for long data
        obj, ifmt = protocol.deserialize(data)
        _log.info('Recv:{!r}: {!r}'.format(self.peer, obj))

        if self.is_processing is True:
            if obj.get('cmd', '') == 'quit-all':
                self.loop.create_task(bus.do_quit(self.loop))
                self.is_processing = False
            bus.qin.put_nowait((self.peer, (ifmt, obj)))

    def send(self, obj, ofmt=None):
        _log.info('Send:{!r}: {!r}'.format(self.peer, obj))
        data = protocol.serialize(obj, ofmt)
        # BAD: exc if client was already deleted when executor was suspended
        self.transport.write(data)
        # CHECK: if need '.drain()' ::: need only for streams


async def sender(conn):
    while True:
        cid, (ofmt, rsp) = await bus.qout.get()
        _log.debug('Response to: {!r}'.format(rsp['id']))
        conn[cid].send(rsp, ofmt)
        bus.qout.task_done()
