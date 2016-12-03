import logging
import asyncio

_log = logging.getLogger(__name__)


# Each queue has dedicated pool of worker task
#   * Fastest independent relay of transit msgs
#   * Exhausting input queue as fast as possible
#   * Send from qout by multiple senders -- if some 'send' socket becomes blocked
qin = asyncio.Queue()
qout = asyncio.Queue()
# qtransit = asyncio.Queue()


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
