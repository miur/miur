import pickle
import logging
import asyncio
# import functools
import threading
# import signal

_clients = {}
_lock = threading.Lock()
_DEFAULT_LIMIT = 2 ** 16
qmsg = asyncio.Queue()

# Quiet poll
# logging.getLogger('asyncio').setLevel(logging.WARNING)


def rememberClient(id, obj):
    with _lock:
        _clients[id] = obj
    logging.info('Client {} remembered'.format(id))


def forgetClient(id):
    with _lock:
        del _clients[id]
    logging.info('Client {} forgotten'.format(id))


def disconnectAll():
    with _lock:
        for c in _clients.values():
            logging.info('Closing the client {!r} socket'.format(c))
            c.transport.close()


# async def sum(x):
#     await asyncio.sleep(0.1)  # simulates asynchronously
#     return x

# async def consumer(i):
#     print("Consumer {} started".format(i))
#     while True:
#         f, x = await q.get()
#         print("Consumer {} procesing {}".format(i, x))
#         r = await sum(x)
#         f.set_result(r)

# async def producer():
#     consumers = [asyncio.ensure_future(consumer(i)) for i in range(5)]
#     # loop = asyncio.get_event_loop()
#     tasks = [(asyncio.Future(), x) for x in range(10)]
#     for task in tasks:
#         await q.put(task)
#     # wait until all futures are completed
#     results = await asyncio.gather(*[f for f, _ in tasks])
#     assert results == [r for _, r in tasks]
#     # destroy tasks
#     for c in consumers:
#         c.cancel()

# asyncio.get_event_loop().run_until_complete(producer())

async def send(id, msg):
    logging.info('Send to {!r}: {!r}'.format(id, msg))
    if isinstance(msg, str):
        data = msg.encode()
    else:
        data = pickle.dumps(msg, protocol=pickle.HIGHEST_PROTOCOL)
    # BAD: exc if client was already deleted when executor was suspended
    _clients[id].transport.write(data)

async def executor():
    while True:
        id, msg = await qmsg.get()
        await send(id, msg)


# ALT: replace by coro handler
#   http://stackoverflow.com/questions/37452039/how-to-correct-asyncio-yield-from-in-data-received
class ClientProtocol(asyncio.Protocol):
    def connection_made(self, transport):
        self.transport = transport
        self.peer = self.transport.get_extra_info('peername')
        logging.info('Connection from {}'.format(self.peer))
        self.id = self.peer  # ALT: use first msg from client as its ID
        rememberClient(self.id, self)

    def connection_lost(self, exc):
        logging.info('Connection lost {}'.format(self.peer))
        if exc is not None:
            raise exc
        forgetClient(self.id)

    def data_received(self, data):
        try:
            # CHECK: 'data_received' guarantee complete msg OR only partial ones
            msg = pickle.loads(data)
        # BAD: unreliable and slow method to combine data + text_msg by 'nc'
        except (pickle.UnpicklingError, KeyError, EOFError):
            msg = data.decode()
        logging.info('Data received: {!r}'.format(msg))
        qmsg.put_nowait((self.id, msg))
        # logging.info('Size: {!r}'.format(qmsg.qsize()))


# def factory(loop):
#     reader = asyncio.StreamReader(limit=_DEFAULT_LIMIT, loop=loop)
#     protocol = asyncio.StreamReaderProtocol(reader, ClientProtocol, loop=loop)
#     return protocol


class EventDriver:
    def __init__(self, server_address, callback):
        self.loop = asyncio.get_event_loop()
        self.loop.set_debug(True)
        # self.loop.add_signal_handler(signal.SIGINT, self.loop.stop)
        # Each client connection will create a new protocol instance
        # coro = (await ...)
        # functools.partial(ClientProtocol, self.loop, callback),
        coro = self.loop.create_server(ClientProtocol, *server_address,
                                       reuse_address=True, reuse_port=True)
        self.server = self.loop.run_until_complete(coro)
        self.actor = self.loop.create_task(executor())
        # SEE self.server.sockets

    def __enter__(self):
        # Serve requests until Ctrl+C is pressed
        logging.info('Serving on {}'.format(self.server.sockets[0].getsockname()))
        return self.loop

    def __exit__(self, *args):
        # BAD?coro -- can't exit from server until all commands processed
        qmsg.join()
        # self.actor.cancel()
        disconnectAll()
        self.server.close()
        self.loop.run_until_complete(self.server.wait_closed())
        # ALT: yield from self.server.wait_closed()
        self.loop.close()
