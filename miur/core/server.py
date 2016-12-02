import logging
import asyncio
# import functools

from miur.share import protocol
# BAD: don't like this circular deps
from . import command
from .client import ClientsList

_log = logging.getLogger(__name__)

# Each queue has dedicated pool of worker task
#   * Fastest independent relay of transit msgs
#   * Exhausting input queue as fast as possible
#   * Send from qout by multiple senders -- if some 'send' socket becomes blocked
qin = asyncio.Queue()
qout = asyncio.Queue()
qtransit = asyncio.Queue()
_cmdd = {v.cmd: v for v in vars(command).values()
         if isinstance(v, type) and issubclass(v, command.BaseCommand)}


def put_in(obj):
    qin.put_nowait(obj)
    _log.debug('Size qin: {!r}'.format(qin.qsize()))


# ALT: replace by coro handler
#   http://stackoverflow.com/questions/37452039/how-to-correct-asyncio-yield-from-in-data-received
class ClientProtocol(asyncio.Protocol):
    _clients = ClientsList()

    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        _log.info('Connection from {}'.format(self.peer))
        self.cid = self.peer  # ALT: use first msg from client as its cid
        ClientProtocol._clients[self.cid] = self

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        del ClientProtocol._clients[self.cid]

    def data_received(self, data):
        obj, ifmt = protocol.deserialize(data)
        _log.info('Recv:{!r}: {!r}'.format(self.cid, obj))
        put_in((self.cid, (ifmt, obj)))

    def send_data(self, obj, ofmt=None):
        _log.info('Send:{!r}: {!r}'.format(self.cid, obj))
        data = protocol.serialize(obj, ofmt)
        # BAD: exc if client was already deleted when executor was suspended
        self.transport.write(data)

    @staticmethod
    async def send(cid, obj, ofmt=None):
        # NOTE: can't await send_data w/o create_task
        return ClientProtocol._clients[cid].send_data(obj, ofmt)


class Bay:
    def __init__(self, server_address, loop):
        self.server_address = server_address
        self.loop = loop

    def __enter__(self):
        # Each client connection will create a new protocol instance
        # functools.partial(ClientProtocol, loop, callback),
        coro = self.loop.create_server(ClientProtocol, *self.server_address,
                                       reuse_address=True, reuse_port=True)
        self.server = self.loop.run_until_complete(coro)
        _log.info('Serving on {}'.format(self.server.sockets[0].getsockname()))

    def __exit__(self, *args):
        ClientProtocol._clients.disconnectAll()
        self.server.close()
        self.loop.run_until_complete(self.server.wait_closed())


async def executor():
    while True:
        cid, (ifmt, obj) = await qin.get()
        _log.debug('Command: {!r}'.format(obj))

        if obj['cmd'] == 'quit-all':
            # TEMP:HACK: reflect 'quit' back to rotate cycle once more until false condition
            r = None
        else:
            # ALT: Register all entries __class__.cmd in dict when loading
            C = _cmdd.get(obj['cmd'])
            c = C(*obj['args'])
            r = c.execute()

        # THINK:WTF: if no such cmd ? Client will hang in infinite loop
        _log.debug('Results: {!r}'.format(r))

        # THINK: instead of directly send msg you can save it to self._msg and
        #   later process this object in send queue
        rsp = {'id': obj['id'], 'rsp': r}
        await ClientProtocol.send(cid, rsp, ifmt)

        if obj['cmd'] == 'quit-all':
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


def main_loop(server_address):
    loop = asyncio.get_event_loop()
    loop.set_debug(True)

    with Bay(server_address, loop):
        # FIXME:RFC Flyover is independent from Bay
        #   => server can reject new conn but continue server already established
        with Flyover(loop) as task:
            loop.run_until_complete(task)
            # loop.run_forever()
            # SEE server.sockets

    loop.close()


def main(server_address):
    try:
        # Serve requests until Ctrl+C is pressed
        main_loop(server_address)
    except KeyboardInterrupt:
        pass
