import logging
import asyncio

from .executor import executor
from . import server

_log = logging.getLogger(__name__)


# FIXME: rm 'Bay' altogether -- *core* may have no server at all when tight linking
#   * ALSO: there may be two sep srv for TCP/UDP simultaneously
# BUT: I used 'with Bay' instead of using 'try: ... finally: ...' for clean-up
class Bay:
    def __init__(self, server_address, loop):
        self.server_address = server_address
        self.loop = loop
        self.conn = server.ClientConnections()

    # TRY: __aenter__ and __aexit__
    def __enter__(self):
        # Each client connection will create a new protocol instance
        # import functools
        # functools.partial(ClientProtocol, loop, callback),
        # SEE self.srv.sockets -- to manipulate raw socket

        # FIX:DEV: await on create_server -- instead of 'run_until_complete(coro)'
        #   => because there may be no server at all for tight coupling (using run_forever())
        coro = self.loop.create_server(
            lambda: server.ClientProtocol(self.loop, self.conn),
            *self.server_address, reuse_address=True, reuse_port=True)
        self.srv = self.loop.run_until_complete(coro)
        _log.info('Serving on {}'.format(self.srv.sockets[0].getsockname()))
        return self.conn

    def __exit__(self, *args):
        self.conn.disconnectAll()
        # CHECK: earlier loop.stop() in do_quit() must not obstruct this ops
        self.srv.close()
        self.loop.run_until_complete(self.srv.wait_closed())


def main_loop(server_address):
    loop = asyncio.get_event_loop()
    loop.set_debug(True)

    with Bay(server_address, loop) as conn:
        # FIXME:RFC task is independent from Bay
        #   => server can reject new conn but continue to serve already established
        loop.create_task(executor())
        loop.create_task(server.sender(conn))
        loop.run_forever()

    loop.close()


def main(server_address):
    try:
        # Serve requests until Ctrl+C is pressed
        main_loop(server_address)
    except KeyboardInterrupt:
        pass
