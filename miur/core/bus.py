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
