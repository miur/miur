import logging
import asyncio
import threading

_log = logging.getLogger(__name__)


# NOTE: mixing ClientProtocol with static vars is BAD idea
# BETTER: re-impl loop.create_server(...) for deterministic order of accept/receive/lost
#   => then parallelism will be controllable and broken lock may become unnecessary
#   BUT: it's currently difficult -- too much to reimplement OR too long to reduce complexity
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
                # sid, cid = dst
                # if sid == sid_grp:
                #     _log.info('Closing the client {!r}'.format(cid))
                    obj.close()

    def ignoreRecvAll(self, sid_grp=None):
        if sid_grp is None:
            sid_grp = self.sid_grp
        # BAD: won't close any connection pending on _lock to in __setitem__
        with self._lock:
            for dst, obj in self.items():
                # sid, cid = dst
                # if sid == sid_grp:
                    obj.ignore_recv_end()


# THINK: accepted/dismissed clients can be passed to Hub through Bus as usual cmds
# TODO: rename to TCPListeningServerConnection to distinguish from outgoing TCPConnection ?
class ClientProtocol(asyncio.Protocol):
    """Each client connection will create a new protocol instance"""

    def __init__(self, conn, make_channel):
        self.make_channel = make_channel
        self.recv_cb = None
        self.conn = conn
        self.is_processing = True
        self.channel = None

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))

        # HACK:
        self.channel = self.make_channel(send_cb=self.send)
        self.recv_cb = self.channel.transport.recv

        # MAYBE: use backward dict [self] = channel :: so I can dismiss self.channel
        # NOTE: adding to dict don't require lock (beside iterating that dict)
        self.conn[self.channel] = self
        # MAYBE: set() is enough BUT how to close _impl connection then ?
        #   => cascade closing of Channel => BUT then you need: channel._impl_conn = self

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        del self.conn[self.channel]

    def close(self):
        _log.info('Closing the client {!r}'.format(self.peer))
        self.transport.close()

    # TRY :: closing recv end instead of ignoring incoming data with 'is_processing'
    def ignore_recv_end(self):
        _log.info('Ignore client data {!r}'.format(self.peer))
        self.is_processing = False

    # SEE:(combine) http://code.activestate.com/recipes/408859-socketrecv-three-ways-to-turn-it-into-recvall/
    # * directly use inner unblocking sockets impl with timeout() instead of looping data_received()
    #   SEE self.srv.sockets -- to manipulate raw socket
    def data_received(self, data):
        _log.debug('Recv:{!r}: {!r} bytes'.format(self.peer, len(data)))
        if not data:
            raise
        if self.is_processing is False:
            return
        self.recv_cb(data)

    # BAD: exc if client was already deleted when executor was suspended
    # CHECK: if need to write multiple times for too big data
    # CHECK:DONE: no need to '.drain()' (used only for streams)
    def send(self, data):
        self.transport.write(data)
