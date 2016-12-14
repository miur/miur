import logging
import asyncio
import socket

from miur.share.osi.basechainlink import BaseChainLink

_log = logging.getLogger(__name__)


# THINK: accepted/dismissed clients can be passed to Hub through Bus as usual cmds
# TODO: rename to TCPListeningServerConnection to distinguish from outgoing TCPConnection ?
class ClientProtocol(asyncio.Protocol, BaseChainLink):
    """Each client connection will create a new protocol instance"""

    def __init__(self, topology):
        self.topology = topology

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))
        self.chan = self.topology.register(self)

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        self.chan = self.topology.deregister(self.chan)

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
        self.sink(data)

    # BAD: exc if client was already deleted when executor was suspended
    # CHECK: if need to write multiple times for too big data
    # CHECK:DONE: no need to '.drain()' (used only for streams)
    def __call__(self, data):
        self.transport.write(data)
