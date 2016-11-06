import logging
import socket
import asyncio

from miur.relay import protocol


_log = logging.getLogger(__name__)

_servers = {}  # task -> (reader, writer)
active_srv = None


def send_once(server_address, obj):
    if isinstance(server_address, tuple):
        family = socket.AF_INET
    else:
        family = socket.AF_UNIX

    with socket.socket(family, socket.SOCK_STREAM) as sock:
        sock.connect(server_address)

        data = protocol.serialize(obj)
        sock.sendall(data)  # ALT: sock.sendfile(f)  # ret nbytes
        _log.info("Sent: {}".format(obj))

        # FIXME: magic 1024 -- how process packets without limitations?
        data = sock.recv(1024)
        obj, _ = protocol.deserialize(data)
        _log.info("Recv: {}".format(obj))

        return obj


# THINK: unite conns from client *mod* and conns to parent *mod*
#   => all conn list used only to write back into socket and disconnect all on quit
#   ? is there need to separate list of 'clients' and 'servers' ?
# TRY: reuse eventdriver/ClientProtocol
#   BUT then 'qin' will contain mix of clients 'ask' and parent 'rsp'
# THINK: is there need for multiple parent conn at once ?
#   Can be treated as such or not ?:
#       ? preview -- as binary data provider in rsp
#       ? stdin provider to load list/selection/yank/etc from stdin directly in cursor ?
#   * At least I need hot-plug to switch between multiple servers
#       => cached dom/frame must be saved independently ?

async def core_connect(server_address, loop):
    global _servers
    reader, writer = await asyncio.open_connection(*server_address, loop=loop)
    _servers[server_address] = (reader, writer)
    _log.info('Connected to {!r}'.format(server_address))


async def core_send(obj):
    global _servers, active_srv
    (_, writer) = _servers[active_srv]
    _log.info('Sent: {!r}'.format(obj))
    data = protocol.serialize(obj)
    writer.write(data)
    await writer.drain()  # MAYBE: need only before 'writer.close()' ?


async def core_recv():
    global _servers, active_srv
    (reader, _) = _servers[active_srv]
    # data = await asyncio.wait_for(reader.readline(), timeout=2.0)
    data = await reader.readline()
    obj, _ = protocol.deserialize(data)
    _log.info('Recv: {!r}'.format(obj))
    return obj


async def core_close():
    global _servers, active_srv
    (_, writer) = _servers[active_srv]
    writer.close()
    del _servers[active_srv]
    _log.info('Disconnected from {!r}'.format(active_srv))


def loop(server_address):
    global active_srv
    active_srv = server_address  # OR another id

    loop = asyncio.get_event_loop()
    loop.run_until_complete(core_connect(server_address, loop))
    loop.run_until_complete(core_send('yes\n'))
    loop.run_until_complete(core_close())
    loop.close()
