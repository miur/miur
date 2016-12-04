import logging
import asyncio

from .executor import executor
from .server import Server

_log = logging.getLogger(__name__)


# NEED:DEV: arch to add/remove servers to event loop on the fly
#   * no srv when tight linking
#   * two srv for simultaneously TCP/UDP
#   ~ srv can reject new conn but continue to serve already established
#   => track all added objects and iterate them on clean up to self.task.cancel()

def main_loop(server_address):
    loop = asyncio.get_event_loop()
    loop.set_debug(True)

    srv = Server(server_address, loop)
    loop.create_task(srv.start())
    loop.create_task(srv.sender())
    loop.create_task(executor())

    try:
        loop.run_forever()
    finally:
        loop.run_until_complete(srv.stop())
        loop.close()


def main(server_address):
    try:
        # Serve requests until Ctrl+C is pressed
        main_loop(server_address)
    except KeyboardInterrupt:
        pass
