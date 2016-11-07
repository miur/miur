import logging
import socket
import asyncio

from miur.relay import protocol


_log = logging.getLogger(__name__)

is_watching = True
_servers = {}  # task -> (reader, writer)
active_srv = None
qcmd = asyncio.Queue()
qdat = asyncio.Queue()


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
    reader, writer = await asyncio.open_connection(*server_address, loop=loop)
    _servers[server_address] = (reader, writer)
    _log.info('Connected to {!r}'.format(server_address))


async def core_send(obj):
    (_, writer) = _servers[active_srv]
    _log.info('Sent: {!r}'.format(obj))
    data = protocol.serialize(obj)
    writer.write(data)
    await writer.drain()  # MAYBE: need only before 'writer.close()' ?


async def core_recv():
    (reader, _) = _servers[active_srv]
    # data = await asyncio.wait_for(reader.readline(), timeout=2.0)
    data = await reader.read()
    obj, _ = protocol.deserialize(data)
    _log.info('Recv: {!r}'.format(obj))
    return obj


async def core_close():
    (_, writer) = _servers[active_srv]
    writer.close()
    del _servers[active_srv]
    _log.info('Disconnected from {!r}'.format(active_srv))


async def qcmd_send():
    while is_watching:
        cmd = await qcmd.get()
        await core_send(cmd)


async def qdat_recv():
    while is_watching:
        obj = await core_recv()
        await qdat.put(obj)
        _log.debug('Size qdat: {!r}'.format(qdat.qsize()))


async def qdat_apply():
    while is_watching:
        obj = await qdat.get()
        print(obj)


def put_cmd(obj):
    qcmd.put_nowait(obj)
    _log.debug('Size qcmd: {!r}'.format(qcmd.qsize()))


async def test():
    global is_watching

    await asyncio.sleep(0.5)
    put_cmd('yes\n')
    await asyncio.sleep(0.5)
    is_watching = False
    await asyncio.sleep(0.5)
    # BAD: need one more outgoing msg to exit all loops
    # BAD: qdat recv won't exit until response (which can never come)
    put_cmd('end\n')
    await asyncio.sleep(0.5)
    put_cmd('bad\n')


def loop(server_address):
    global active_srv

    active_srv = server_address  # OR another id

    loop = asyncio.get_event_loop()
    loop.run_until_complete(core_connect(server_address, loop))
    tasks = [loop.create_task(qcmd_send()),
             loop.create_task(qdat_recv()),
             loop.create_task(qdat_apply()),
             loop.create_task(test())]
    loop.run_until_complete(asyncio.gather(*tasks))
    loop.run_until_complete(core_close())

    loop.close()
