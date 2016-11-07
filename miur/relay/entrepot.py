import logging
import asyncio

from miur.relay.eventdriver import ClientProtocol

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
        await ClientProtocol.send(cid, obj, ifmt)
        if obj == 'quit_all':
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
