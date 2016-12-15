import logging
import asyncio
import uuid

from . import bus

_log = logging.getLogger(__name__.split('.', 2)[1])


# NEED:DEV: arch to add/remove servers to event loop on the fly
#   * no srv when tight linking
#   * two srv for simultaneously TCP/UDP
#   ~ srv can reject new conn but continue to serve already established
#   => track all added objects and iterate them on clean up to self.task.cancel()

# FIXME: instead of passing 'server_address' as args -- merge args with config
#   registry and then launch using registry only => more decoupling
class CoreProgramm:
    def __init__(self, server_address):
        # NOTE: each *mod* instance must have different uuid to address itself
        #   * even when all *mods* are linked in single program !
        #   => that uuid is transfered in all forked/threaded *mods*
        #     => originally linked ones become disfunct and are deinitialized
        self.uuid = str(uuid.uuid4())
        self.loop = asyncio.get_event_loop()
        self.loop.set_debug(True)
        self.top = bus.Topology(server_address, ctx=self, loop=self.loop)
        self.run()

    def run(self):
        try:
            self.loop.run_forever()
        finally:
            self.loop.run_until_complete(self.top.quit_clean())
            self.loop.close()


def main(server_address):
    try:
        # Serve requests until Ctrl+C is pressed
        CoreProgramm(server_address)
    except KeyboardInterrupt:
        pass
