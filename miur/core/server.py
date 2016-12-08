import struct
import logging
import asyncio
import threading
import functools

_log = logging.getLogger(__name__)


# NOTE: mixing ClientProtocol with static vars is BAD idea
# BETTER: re-impl loop.create_server(...) for deterministic order of accept/receive/lost
#   => then parallelism will be controllable and broken lock may become unnecessary
# NOTE: list of connections must be sep entity
#   << if you create two separate servers -- one for TCP and another for UDP
#       -- to support two diff clients lists
class ClientConnections(dict):
    def __init__(self, sid_grp=None):
        self._lock = threading.Lock()
        self.sid_grp = sid_grp

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
    def disconnectAll(self, sid_grp=None):
        if sid_grp is None:
            sid_grp = self.sid_grp
        # BAD: won't close any connection pending on _lock to in __setitem__
        with self._lock:
            for dst, obj in self.items():
                sid, cid = dst
                if sid == sid_grp:
                    _log.info('Closing the client {!r}'.format(cid))
                    obj.close()

    def ignoreRecvAll(self, sid_grp=None):
        if sid_grp is None:
            sid_grp = self.sid_grp
        # BAD: won't close any connection pending on _lock to in __setitem__
        with self._lock:
            for dst, obj in self.items():
                sid, cid = dst
                if sid == sid_grp:
                    obj.ignore_recv_end()


class ClientProtocol(asyncio.Protocol):
    """Each client connection will create a new protocol instance"""
    h_sz_len = 4

    def __init__(self, bus, conn):
        self.bus = bus
        self.conn = conn
        self.is_processing = True
        self._n = ClientProtocol.h_sz_len
        self._buf = bytearray()
        self._head = True

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))
        # CHG:USE: self.dst = (type(self.__class__), server.peer, self.peer, protocol)
        #   * NOTE: do support multiple instances of class: (self, (socket))
        self.dst = ('tcp', self.peer)

        # EXPL: ignore incoming connections when server is shutting down
        # BAD: must be placed inside Server() -- otherwise new conn accumulates
        if self.is_processing is True:
            # NOTE: adding to dict don't require lock (beside iterating that dict)
            self.conn[self.dst] = self
            # ALT: use first msg from client as its cid
        else:
            self.close()

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        del self.conn[self.dst]

    def close(self):
        _log.info('Closing the client {!r}'.format(self.peer))
        self.transport.close()

    def ignore_recv_end(self):
        _log.info('Ignore client data {!r}'.format(self.peer))
        self.is_processing = False

    # SEE:(combine) http://code.activestate.com/recipes/408859-socketrecv-three-ways-to-turn-it-into-recvall/
    # * directly use inner unblocking sockets impl with timeout() instead of looping data_received()
    #   SEE self.srv.sockets -- to manipulate raw socket
    #   TRY :: closing recv end instead of ignoring incoming data with 'is_processing'
    # * periodically send 'heartbeat' data and drop incomplete msg on each
    #   heartbeat, raising error or requesting msg re-send from client
    # RFC:FIX: SEP mixed concepts of transport and protocol
    def data_received(self, data):
        _log.debug('Recv:{!r}: {!r} bytes'.format(self.peer, len(data)))
        if not data:
            raise
        if self.is_processing is False:
            return
        # Accumulate data even if too small for both branches
        self._buf = self.parse_msg(self._buf + data)

    def parse_msg(self, buf):
        i = 0
        # NOTE: used single cycle to process multiple msgs received at once
        while (i + self._n) <= len(buf):
            blob = buf[i:i + self._n]
            i += self._n
            if self._head:
                self._n = struct.unpack('>I', blob)[0]
            else:
                self._n = ClientProtocol.h_sz_len
                self.bus.put_cmd(self.dst, blob)
            self._head = not self._head
        return buf[i:]

    def send(self, data):
        # BAD: exc if client was already deleted when executor was suspended
        # CHECK: if need to write multiple times for too big data
        header = struct.pack('>I', len(data))
        self.transport.write(header + data)
        # CHECK: if need '.drain()' ::: need only for streams


class Server:
    def __init__(self, server_address, loop, bus):
        self.server_address = server_address
        self.loop = loop
        self.bus = bus
        self._asyncio_server = None
        self.conn = ClientConnections(sid_grp=self)

    async def start(self):
        self._asyncio_server = await self.loop.create_server(
            functools.partial(ClientProtocol, self.bus, self.conn),
            *self.server_address, reuse_address=True, reuse_port=True)
        peer = self._asyncio_server.sockets[0].getsockname()
        _log.info('Serving on {}'.format(peer))

    async def stop(self):
        self.conn.disconnectAll('tcp')
        # CHECK: earlier loop.stop() in do_quit() must not obstruct this ops
        self._asyncio_server.close()
        await self._asyncio_server.wait_closed()
