#
# SPDX-FileCopyrightText: 2016 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
# IDEA: replace 'core/server' by coro handler
#   http://stackoverflow.com/questions/37452039/how-to-correct-asyncio-yield-from-in-data-received
# BAD: there no 'connection_lost' callback beside polling on 'reader.at_eof()'
# SEE: https://github.com/python/asyncio/issues/95
#   If you use drain() (like you should) you should see the exception, also
#   whenever you call read() it will return b'' if it was a "clean" disconnect
#   or raise an exception if not.
# USE: https://stackoverflow.com/questions/31077182/python-asyncio-streaming-api
#   SUM:(cancel) no big diff to asyncio.Protocol itself => no need for this many abstractions

# NOTE:FIND: seems like asyncio transports already has necessary queue facilities for receive/send queues ?
#   => there are (reader, writer), but I can't use it as bus for cmds
# ALSO: check if create_server already has analogue to '_clients' to track all connections
#   => Nope, it only counts number of connections, no more

import logging
import asyncio
import threading

_log = logging.getLogger(__name__)
_lock = threading.Lock()
_clients = {}


class MultiServer(asyncio.StreamReaderProtocol):

    def connection_made(self, transport):
        super().connection_made(transport)

    def connection_lost(self, exc):
        _log.info('Connection lost {}'.format(self.peer))
        super().connection_lost(exc)
        if exc is not None:
            raise exc
        with _lock:
            # BAD: crash if waits on _lock until disconnectAll() exits
            # BUG: can't save self.cid in this class -- when multiple clients connect
            del _clients[self.cid]

async def accept_client(reader, writer):
    # ALT: use first msg from client as its cid
    cid = writer.get_extra_info('peername')
    _log.info('Connection from {}'.format(cid))
    with _lock:
        _clients[cid] = (reader, writer)


async def start_server(self, loop):
    def factory():
        return MultiServer(asyncio.StreamReader(), accept_client)
    self.server = yield from loop.create_server(factory, self.host, self.port)
