import logging
import asyncio
import uuid

from .bus import Bus
from .server import Server
from .command import CommandMaker

_log = logging.getLogger(__name__)


async def cmd_executor(msg_bus):
    while True:
        car = await msg_bus.qin.get()
        # CHG: use global executor class for this
        msg_bus.execute(car)
        msg_bus.qin.task_done()


# THINK: how to hide inner impl qin/qout from coro ? return task to wait on ?
async def rsp_dispatcher(msg_bus, all_conn):
    while True:
        car = await msg_bus.qout.get()
        msg_bus.pop_rsp(car, all_conn)
        msg_bus.qout.task_done()


# NEED:DEV: arch to add/remove servers to event loop on the fly
#   * no srv when tight linking
#   * two srv for simultaneously TCP/UDP
#   ~ srv can reject new conn but continue to serve already established
#   => track all added objects and iterate them on clean up to self.task.cancel()

class CoreProgramm:
    def __init__(self, server_address):
        # NOTE: each *mod* instance must have different uuid to address itself
        #   * even when all *mods* are linked in single program !
        #   => that uuid is transfered in all forked/threaded *mods*
        #     => originally linked ones become disfunct and are deinitialized
        self.uuid = str(uuid.uuid4())
        self.init(server_address)
        self.run()

    def init(self, server_address):
        self.loop = asyncio.get_event_loop()
        self.loop.set_debug(True)
        self.make_cmd = CommandMaker('miur.core.command.all').make
        self.bus = Bus(self.make_cmd, ctx=self)
        self.srv = Server(server_address, self.loop, self.bus)
        self.loop.create_task(self.srv.start())
        self.loop.create_task(rsp_dispatcher(self.bus, self.srv.conn))
        self.loop.create_task(cmd_executor(self.bus))

    def run(self):
        try:
            self.loop.run_forever()
        finally:
            self.loop.run_until_complete(self.srv.stop())
            self.loop.close()


def main(server_address):
    try:
        # Serve requests until Ctrl+C is pressed
        CoreProgramm(server_address)
    except KeyboardInterrupt:
        pass
