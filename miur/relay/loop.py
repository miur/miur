import logging
import asyncio
# import signal

from miur.relay import entrepot, eventdriver

_log = logging.getLogger(__name__)


# _DEFAULT_LIMIT = 2 ** 16
# def factory(loop):
#     reader = asyncio.StreamReader(limit=_DEFAULT_LIMIT, loop=loop)
#     protocol = asyncio.StreamReaderProtocol(reader, ClientProtocol, loop=loop)
#     return protocol


# loop.add_signal_handler(signal.SIGINT, loop.stop)
def main_loop(server_address):
    loop = asyncio.get_event_loop()
    loop.set_debug(True)

    with eventdriver.Bay(server_address, loop):
        with entrepot.Flyover(loop) as task:
            loop.run_until_complete(task)
            # loop.run_forever()
            # SEE server.sockets

    loop.close()
