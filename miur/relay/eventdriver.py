import pickle
import logging
import asyncio
import functools
# import signal

clients = []
_DEFAULT_LIMIT = 2 ** 16


class ClientProtocol(asyncio.Protocol):
    def __init__(self, loop, callback):
        self.loop = loop
        self.callback = callback

    def connection_made(self, transport):
        peername = transport.get_extra_info('peername')
        logging.info('Connection from {}'.format(peername))
        self.transport = transport

    # TODO: how writing to socket looks like ?
    def data_received(self, data):
        message = pickle.loads(data)
        logging.info('Data received: {!r}'.format(message))

        self.callback(message)

        logging.info('Send: {!r}'.format(message))

        response = pickle.dumps(message, protocol=pickle.HIGHEST_PROTOCOL)
        # FIND how to call it from outside
        self.transport.write(response)

        logging.info('Close the client socket')
        self.transport.close()  # TEMP:REM

    # def connection_lost(self, exc):
        # TEMP:REM: one-time connection
        # self.loop.stop()


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
        coro = self.loop.create_server(
            functools.partial(ClientProtocol, self.loop, callback),
            *server_address, reuse_address=True, reuse_port=True)
        self.server = self.loop.run_until_complete(coro)
        # SEE self.server.sockets

    def __enter__(self):
        # Serve requests until Ctrl+C is pressed
        logging.info('Serving on {}'.format(self.server.sockets[0].getsockname()))
        return self.loop

    def __exit__(self, *args):
        self.server.close()
        self.loop.run_until_complete(self.server.wait_closed())
        # ALT: yield from self.server.wait_closed()
        self.loop.close()
