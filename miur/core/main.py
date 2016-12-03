import logging
import asyncio

from .executor import executor
from .server import ClientProtocol, sender

_log = logging.getLogger(__name__)


class Bay:
    def __init__(self, server_address, loop):
        self.server_address = server_address
        self.loop = loop

    # TRY: __aenter__ and __aexit__
    def __enter__(self):
        # Each client connection will create a new protocol instance
        # functools.partial(ClientProtocol, loop, callback),
        # SEE self.server.sockets -- to manipulate raw socket
        coro = self.loop.create_server(lambda: ClientProtocol(self.loop),
                                       *self.server_address,
                                       reuse_address=True, reuse_port=True)
        self.server = self.loop.run_until_complete(coro)
        _log.info('Serving on {}'.format(self.server.sockets[0].getsockname()))

    def __exit__(self, *args):
        ClientProtocol.disconnectAll()
        # CHECK: earlier loop.stop() in do_quit() must not obstruct this ops
        self.server.close()
        self.loop.run_until_complete(self.server.wait_closed())


def main_loop(server_address):
    loop = asyncio.get_event_loop()
    loop.set_debug(True)

    with Bay(server_address, loop):
        # FIXME:RFC task is independent from Bay
        #   => server can reject new conn but continue to serve already established
        loop.create_task(executor())
        loop.create_task(sender())
        loop.run_forever()

    loop.close()


def main(server_address):
    try:
        # Serve requests until Ctrl+C is pressed
        main_loop(server_address)
    except KeyboardInterrupt:
        pass
