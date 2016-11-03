import pickle
import asyncio
import logging

clients = []


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
        self.transport.write(response)

        logging.info('Close the client socket')
        self.transport.close()

    def connection_lost(self, exc):
        # TEMP:REM: one-time connection
        self.loop.stop()


class EventDriver:
    def __init__(self, server_address, callback):
        ip, port = server_address
        self.loop = asyncio.get_event_loop()
        # Each client connection will create a new protocol instance
        coro = self.loop.create_server(
            lambda: ClientProtocol(self.loop, callback), ip, port,
            reuse_address=True, reuse_port=True)
        self.server = self.loop.run_until_complete(coro)
        ## loop.set_debug()

    def __enter__(self):
        # Serve requests until Ctrl+C is pressed
        logging.info('Serving on {}'.format(self.server.sockets[0].getsockname()))
        return self.loop

    def __exit__(self, *args):
        self.server.close()
        self.loop.run_until_complete(self.server.wait_closed())
        self.loop.close()
