import logging
import asyncio

from .eventdriver import ClientProtocol
from . import executor as exe

_log = logging.getLogger(__name__)

# Each queue has dedicated pool of worker task
#   * Fastest independent relay of transit msgs
#   * Exhausting input queue as fast as possible
#   * Send from qout by multiple senders -- if some 'send' socket becomes blocked
qin = asyncio.Queue()
qout = asyncio.Queue()
qtransit = asyncio.Queue()


def put_in(obj):
    qin.put_nowait(obj)
    _log.debug('Size qin: {!r}'.format(qin.qsize()))


async def executor():
    while True:
        cid, (ifmt, obj) = await qin.get()
        _log.debug('Command: {!r}'.format(obj))
        c = obj['cmd']
        # Register all entries __class__.cmd in dict when loading
        if c == 'get.node.parent':
            r = exe.NodeGetParent().process(obj)
        elif c == 'get.node.child':
            r = exe.NodeGetChild().process(obj)
        elif c == 'list.node':
            r = exe.ListNode().process(obj)
        elif c == 'quit-all':
            # TEMP:HACK: reflect 'quit' back to rotate cycle once more
            #   until false condition
            r = obj
        # THINK:WTF: if no such cmd ? Client will hang in infinite loop
        _log.debug('Results: {!r}'.format(r))
        await ClientProtocol.send(cid, r, ifmt)
        if c == 'quit-all':
            break


class Flyover:
    global qin

    def __init__(self, loop):
        self.loop = loop

    # TRY: __aenter__ and __aexit__
    def __enter__(self):
        self.task = self.loop.create_task(executor())
        return self.task

    def __exit__(self, *args):
        # BAD?coro -- can't exit from server until all commands processed
        # BAD! if clients continue put new msgs in queue -- it will never exit!
        #   => must close receiving end of socket
        #       --> client won't be able to add msgs and will know that socket closed
        #   BUT: what if we want to cancel 'quit'?
        #       => No sense :: all funcs already jumped to 'shutdown' state

        qin.join()  # must process all input msg

        # DEV: remove added task from
        # SEE: http://stackoverflow.com/questions/34710835/proper-way-to-shutdown-asyncio-tasks
        # self.task.cancel()
