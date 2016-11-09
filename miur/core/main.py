import logging
import asyncio
# import signal

from . import entrepot, eventdriver

# Quiet poll
# logging.getLogger('asyncio').setLevel(logging.WARNING)
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
        # FIXME:RFC Flyover is independent from Bay
        #   => server can reject new conn but continue server already established
        with entrepot.Flyover(loop) as task:
            loop.run_until_complete(task)
            # loop.run_forever()
            # SEE server.sockets

    loop.close()


def main(server_address):
    # DEV: pass mod-specific code into main_loop
    try:
        # Serve requests until Ctrl+C is pressed
        main_loop(server_address)
    except KeyboardInterrupt:
        pass
