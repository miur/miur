import logging
import asyncio
import socket

_log = logging.getLogger(__name__)


# THINK: accepted/dismissed clients can be passed to Hub through Bus as usual cmds
# TODO: rename to TCPListeningServerConnection to distinguish from outgoing TCPConnection ?
class ClientProtocol(asyncio.Protocol):
    """Each client connection will create a new protocol instance"""

    def __init__(self, channels, make_channel):
        self.channels = channels
        self.make_channel = make_channel
        self.recv_cb = None
        self.chan = None

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))

        # HACK: no encapsulation :: make channel.get_recv_pt() ?
        self.chan = self.make_channel(conn=self)
        self.recv_cb = self.chan.transport.recv

        # MAYBE: use backward dict [self] = channel :: so I can dismiss self.chan
        # NOTE: adding to dict don't require lock (beside iterating that dict)
        # MAYBE: set() is enough BUT how to close _impl connection then ?
        #   => cascade closing of Channel => BUT then you need: channel._impl_conn = self
        #   BETTER: cascading :: allows to mid-close channel and replace transport
        self.channels.add(self.chan)

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        self.channels.remove(self.chan)

    def close(self):
        _log.info('Closing the client {!r}'.format(self.peer))
        self.transport.close()

    def close_recv(self):
        _log.info('Closing receiving end {!r}'.format(self.peer))
        # CHECK: if closing recv end truly works -- by scheduling and sending one more msg from client
        self.transport.get_extra_info('socket').shutdown(socket.SHUT_RD)
        # ALT: self.is_processing = False

    # SEE:(combine) http://code.activestate.com/recipes/408859-socketrecv-three-ways-to-turn-it-into-recvall/
    # * directly use inner unblocking sockets impl with timeout() instead of looping data_received()
    #   SEE self.srv.sockets -- to manipulate raw socket
    def data_received(self, data):
        _log.debug('Recv:{!r}: {!r} bytes'.format(self.peer, len(data)))
        if not data:
            raise
        self.recv_cb(data)

    # BAD: exc if client was already deleted when executor was suspended
    # CHECK: if need to write multiple times for too big data
    # CHECK:DONE: no need to '.drain()' (used only for streams)
    def send(self, data):
        self.transport.write(data)
