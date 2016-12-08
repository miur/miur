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


# NOTE: adapted to bus, works in both dir, aggregates sep concepts of subsystems
class Carrier:
    def __init__(self, dst=None, cmd=None, rsp=None, fmt=None, uid=None):
        self.dst = dst  # pair (sid, cid) -- listening server + accepted client
        self.cmd = cmd
        self.rsp = rsp  # THINK: 'rsp' can be 'cmd' if initiated from *core* to *client*
        self.fmt = fmt  # MAYBE: excessive and must be removed
        self.uid = uid  # unique message id


# NOTE: there is sense in constructing cmds directly on receiving
#   => so I can immediately construct QuitMsg and then decide when to execute it
#       (immediately or after rest of queue -- to support quit_now and graceful_exit)
#   =>> then executor() will be only calling '.execute()' method and qout.put()
def put_cmd(dst, data_whole):
    global qin
    obj, ifmt = protocol.deserialize(data_whole)
    _log.debug('Packet({!r}b): {!r}'.format(len(data_whole), obj))
    cmd = command.make_cmd(obj['cmd'], *obj['args'])
    car = Carrier(dst, cmd, fmt=ifmt, uid=obj['id'])
    qin.put_nowait(car)
    return car


# THINK: pass 'qin, qout' as args
async def cmd_executor():
    global qin, qout
    while True:
        car = await qin.get()
        _log.debug('Command: {!r}'.format(car.cmd.cmd))
        car.rsp = car.cmd.execute()
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


# USE run_forever() and schedule do_quit() on 'quit' cmd
# MOVE: to fully fledged command QuitCmd
#   BUT:WTF if QuitCmd instance will be destroyed before do_quit() called/finish?
async def do_quit(loop):
    """Exit from server only when all cmds in queues processed """
    global qin, qout

    # NOTE: qin is already exhausted -- shutdown message is always the last
    # one -- and it triggers closing of executor()
    await qin.join()

    # BAD! wait qout only after all executors done!
    #   => qin.pop() is immediate, but qout.put() is often delayed until cmd finished
    # TRY: use semaphore with timer ?
    await qout.join()

    # THINK: place 'shutdown' into qout and wait again or send 'shutdown' immediately from server ?
    #   => must traverse 'shutdown' through whole system to establish proper shutdown chain
    # WARN! must be the last action !  No more async coro after this !
    loop.stop()
