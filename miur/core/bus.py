import logging
import asyncio

from miur.share import protocol

from . import command

_log = logging.getLogger(__name__)


# Each queue has dedicated pool of worker task
#   * Fastest independent relay of transit msgs
#   * Exhausting input queue as fast as possible
#   * Send from qout by multiple senders -- if some 'send' socket becomes blocked
qin = asyncio.Queue()
qout = asyncio.Queue()
# qtransit = asyncio.Queue()

# FIX: must be passed from main() to ???
make_cmd = command.CommandMaker('miur.core.command.all').make


# NOTE: adapted to bus, works in both dir, aggregates sep concepts of subsystems
class Carrier:
    def __init__(self, dst=None, cmd=None, rsp=None, fmt=None, uid=None):
        self.dst = dst  # pair (sid, cid) -- listening server + accepted client
        self.cmd = cmd
        self.rsp = rsp  # THINK: 'rsp' can be 'cmd' if initiated from *core* to *client*
        self.fmt = fmt  # MAYBE: excessive and must be removed
        self.uid = uid  # unique message id

    # WTF if cmd will be executed second time ?
    #   THINK: Allow or disable availability
    def execute(self, *args):
        self.rsp = self.cmd.execute(*args)
        return self


# NOTE: there is sense in constructing cmds directly on receiving
#   => so I can immediately construct QuitMsg and then decide when to execute it
#       (immediately or after rest of queue -- to support quit_now and graceful_exit)
#   =>> then executor() will be only calling '.execute()' method and qout.put()

# FIXME: design how to catch/distribute global ctx like self.loop to cmds ?
# MAYBE: pass self.loop as main()->CommandMaker and there pass it to each cmds on __init__
def put_cmd(dst, data_whole, **kw):
    global qin
    obj, ifmt = protocol.deserialize(data_whole)
    _log.debug('Packet({!r}b): {!r}'.format(len(data_whole), obj))
    cmd = make_cmd(obj['cmd'], *obj['args'])
    car = Carrier(dst, cmd, fmt=ifmt, uid=obj['id'])

    policy = getattr(car.cmd, 'policy', command.GENERAL)
    if policy == command.GENERAL:
        qin.put_nowait(car)
    elif policy == command.IMMEDIATE:
        car.execute(kw['loop'])
        qout.put_nowait(car)

    return car


# THINK: pass 'qin, qout' as args
async def cmd_executor():
    global qin, qout
    while True:
        car = await qin.get()
        _log.debug('Command: {!r}'.format(car.cmd.cmd))
        car.execute()
        qout.put_nowait(car)
        qin.task_done()


async def rsp_dispatcher(all_conn):
    global qout
    while True:
        car = await qout.get()
        rsp = {'id': car.uid, 'rsp': car.rsp}
        data = protocol.serialize(rsp, car.fmt)
        _log.info('Response({:x}): {!r}'.format(car.uid, rsp['rsp']))
        all_conn[car.dst].send(data)
        qout.task_done()
