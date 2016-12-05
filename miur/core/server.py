import struct
import logging
import asyncio
import threading
import functools

from . import bus

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
            _log.info('Client {!r} removed'.format(cid))
            return super().__delitem__(cid)

    # ALT: overload generator :: [c.close() for c in conn.values()]
    def disconnectAll(self, sid_grp):
        # BAD: won't close any connection pending on _lock to in __setitem__
        with self._lock:
            for dst, obj in self.items():
                sid, cid = dst
                if sid == sid_grp:
                    _log.info('Closing the client {!r}'.format(cid))
                    obj.close()


class ClientProtocol(asyncio.Protocol):
    """Each client connection will create a new protocol instance"""

    def __init__(self, loop, conn):
        self.loop = loop
        self.conn = conn
        self.is_processing = True
        self._len = None
        self._buf = bytearray()

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))
        # CHG:USE: self.dst = (type(self.__class__), server.peer, self.peer, protocol)
        #   * NOTE: do support multiple instances of class: (self, (socket))
        self.dst = ('tcp', self.peer)
        # EXPL: ignore incoming connections when server is shutting down
        if self.is_processing is True:
            # NOTE: adding to dict don't require lock (beside iterating that dict)
            self.conn[self.dst] = self
            # ALT: use first msg from client as its cid

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        del self.conn[self.dst]

    def close(self):
        _log.info('Closing the client {!r}'.format(self.peer))
        self.transport.close()

    # NEED:DEV: loop for long data -- save incomplete parts into self.buf
    #   SEE:(combine) http://code.activestate.com/recipes/408859-socketrecv-three-ways-to-turn-it-into-recvall/
    # RFC:FIX: SEP mixed concepts of transport and protocol
    # SEE self.srv.sockets -- to manipulate raw socket
    #   :: like closing receiving end instead of ignoring incoming data
    def data_received(self, data):
        _log.debug('Recv:{!r}: {!r} bytes'.format(self.peer, len(data)))
        if not data:
            raise
        self._buf += data

        # ENH:(inefficient) optimize loop with branches and slicing
        while True:
            if self._len is None:
                if len(self._buf) < 4:
                    break
                size = self._buf[:4]
                self._buf = self._buf[4:]
                self._len = struct.unpack('>I', size)[0]
            else:
                if len(self._buf) < self._len:
                    break
                msg = self._buf[:self._len]
                self._buf = self._buf[self._len:]
                self._len = None
                self.receive(msg)

    def receive(self, msg):
        if self.is_processing is True:
            # TODO: pass 'put_cmd' as arg
            car = bus.put_cmd(self.dst, msg)
            # THINK:TODO: move into 'cmd_executor'
            if type(car.cmd) == bus.command.QuitCmd:
                self.loop.create_task(bus.do_quit(self.loop))
                self.is_processing = False

    def send(self, data):
        # BAD: exc if client was already deleted when executor was suspended
        # CHECK: if need to write multiple times for too big data
        header = struct.pack('>I', len(data))
        self.transport.write(header + data)
        # CHECK: if need '.drain()' ::: need only for streams


class Server:
    def __init__(self, server_address, loop):
        self.server_address = server_address
        self.loop = loop
        self._asyncio_server = None
        self.conn = ClientConnections()

    async def start(self):
        self._asyncio_server = await self.loop.create_server(
            functools.partial(ClientProtocol, self.loop, self.conn),
            *self.server_address, reuse_address=True, reuse_port=True)
        peer = self._asyncio_server.sockets[0].getsockname()
        _log.info('Serving on {}'.format(peer))

    async def stop(self):
        self.conn.disconnectAll('tcp')
        # CHECK: earlier loop.stop() in do_quit() must not obstruct this ops
        self._asyncio_server.close()
        await self._asyncio_server.wait_closed()
