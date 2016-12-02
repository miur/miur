import logging
import asyncio

from . import bus
from .executor import executor
from .server import ClientProtocol

_log = logging.getLogger(__name__)


class Bay:
    def __init__(self, server_address, loop):
        self.server_address = server_address
        self.loop = loop

    # TRY: __aenter__ and __aexit__
    def __enter__(self):
        # Each client connection will create a new protocol instance
        # functools.partial(ClientProtocol, loop, callback),
        coro = self.loop.create_server(ClientProtocol, *self.server_address,
                                       reuse_address=True, reuse_port=True)
        self.server = self.loop.run_until_complete(coro)
        _log.info('Serving on {}'.format(self.server.sockets[0].getsockname()))

    def __exit__(self, *args):
        ClientProtocol.disconnectAll()
        self.server.close()
        self.loop.run_until_complete(self.server.wait_closed())


def main_loop(server_address):
    loop = asyncio.get_event_loop()
    loop.set_debug(True)

    with Bay(server_address, loop):
        # FIXME:RFC task is independent from Bay
        #   => server can reject new conn but continue server already established
        task = loop.create_task(executor())

        loop.run_until_complete(task)
        # loop.run_forever()
        # SEE server.sockets

        # BAD?coro -- can't exit from server until all commands processed
        #   BUT! executor already exited. MAYBE move qin.join() to on_quit event ?
        # BAD! if clients continue put new msgs in queue -- it will never exit!
        #   => must close receiving end of socket
        #       --> client won't be able to add msgs and will know that socket closed
        #   BUT: what if we want to cancel 'quit'?
        #       => No sense :: all funcs already jumped to 'shutdown' state
        bus.qin.join()  # must process all input msg

        # THINK:DEV: remove added task from
        # SEE: http://stackoverflow.com/questions/34710835/proper-way-to-shutdown-asyncio-tasks
        # self.task.cancel()

    loop.close()


def main(server_address):
    try:
        # Serve requests until Ctrl+C is pressed
        main_loop(server_address)
    except KeyboardInterrupt:
        pass
