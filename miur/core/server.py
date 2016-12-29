import logging
import asyncio
import socket

from miur.share import ifc

_log = logging.getLogger(__name__.split('.', 2)[1])


# THINK: accepted/dismissed clients can be passed to Hub through Bus as usual cmds
# TODO: rename to TCPListeningServerConnection to distinguish from outgoing TCPConnection ?
# ALT:(name): TcpTransport
# NOTE:ATT: actually, even connection is ILink ::: MOVE this desc into ifc.py
#   * code body from call() till sink() is splitted apart into write() and data_received()
#   * it even can be moved into two heterogeneous classes, keeping single ifc point in each
#       -- however, you can inherit whole ifc and simply keep 2nd ifc point NotImplemented
#   * execution is deffered by net delays when synchronously blocking
#   * being asynchronous otherwise with its own thread/coro
class ClientProtocol(ifc.Socket, asyncio.Protocol):
    """Each client connection will create a new protocol instance"""

    def __init__(self, hub):
        self.hub = hub
        super().__init__(inner=self.send)

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))

        # TODO: don't make channel itself ! Don't keep ref to 'hub' !
        #   * post msg in bus to create channel
        #   * wait until channel connected to here
        self.chan = self.hub.make_channel(rhs=self)

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        self.hub.unbind(self.chan)
        self.chan = None

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
        self.slot(data)

    # BAD: exc if client was already deleted when executor was suspended
    # CHECK: if need to write multiple times for too big data
    # CHECK:DONE: no need to '.drain()' (used only for streams)
    def send(self, data):
        self.transport.write(data)
