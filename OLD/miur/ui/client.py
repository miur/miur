#
# SPDX-FileCopyrightText: 2016 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
import time
import struct
import logging
import asyncio
import threading

from miur.share import protocol
from miur.cursor import effect


_log = logging.getLogger(__name__.split('.', 2)[1])
_thread_loop = None
is_initialized = threading.Semaphore(0)
is_watching = True

_servers = {}  # task -> (reader, writer)
active_srv = None
qcmd = None
qdat = None

tmp_sending = threading.Semaphore(0)
dwait = {}
counter = 0
_effd = {v.cmd: v for v in vars(effect).values()
         if isinstance(v, type) and issubclass(v, effect.BaseEffect)}


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
    # BAD: limitation to 4Gb messages
    header = struct.pack('>I', len(data))
    writer.write(header + data)
    await writer.drain()  # MAYBE: need only before 'writer.close()' ?


async def core_recv():
    (reader, _) = _servers[active_srv]
    # data = await asyncio.wait_for(reader.readline(), timeout=2.0)
    # DEV:TODO:FIXME:BAD: magic size
    #   -- need continuous read and reconstruct whole packet (/usr/lib is big!)
    #   !!> pickle will raise exception on partial data
    #   + split whole state diff to many small independent state frame diffs fitting inside single packet
    #       => losing single frame diff one won't increase significantly total latency of command
    #   ALT: send size+delim+pickle and combine buf in 'while s < S: s += recv()'
    #       https://bytes.com/topic/python/answers/894052-sending-pickled-dictionary-over-socket
    header = await reader.read(4)
    if not header:
        raise
        # return None
    data_len = struct.unpack('>I', header)[0]
    data = await reader.read(data_len)
    obj, _ = protocol.deserialize(data)
    _log.info('Recv: {!r}'.format(obj))
    return obj


async def core_close():
    (_, writer) = _servers[active_srv]
    writer.close()
    del _servers[active_srv]
    _log.info('Disconnected from {!r}'.format(active_srv))


def sign_msg_from_cmd(cmd):
    # NOTE: this is similar to 'watermarking' packet with ip address
    global dwait, counter
    counter += 1
    h = hash((counter, time.clock()))
    # Incapsulated requests wait on rsp
    dwait[h] = cmd
    m = cmd.msg()
    m['id'] = h
    return m


async def qcmd_send():
    while is_watching:
        cmd = await qcmd.get()
        m = sign_msg_from_cmd(cmd)
        await core_send(m)
        qcmd.task_done()
        # ALT:
        # if cmd == 'quit_all':
        #     break


async def qdat_recv():
    while is_watching:
        obj = await core_recv()
        qdat.put_nowait(obj)
        _log.debug('Size qdat: {!r}'.format(qdat.qsize()))


def process_rsp(obj):
    h = obj['id']
    m = dwait.pop(h)        # search sent msg by hash
    E = _effd.get(m.cmd)    # search eff applier in module
    e = E(m, obj['rsp'])
    # Global state change. NEED: Lock
    e.apply()
    tmp_sending.release()


async def qdat_apply():
    while is_watching:
        obj = await qdat.get()
        _log.warning('Dat: {!r}'.format(obj))
        process_rsp(obj)
        qdat.task_done()


def put_cmd(obj):
    qcmd.put_nowait(obj)
    _log.debug('Size qcmd: {!r}'.format(qcmd.qsize()))


def put_cmd_threadsafe(obj):
    # WARN:NEED: wait until 'qcmd' initialized
    _thread_loop.call_soon_threadsafe(put_cmd, obj)
    # TEMP: sync on waiting cursor.path new value
    tmp_sending.acquire()


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


def new_loop():
    global qcmd, qdat, _thread_loop
    # BUG: There is no current event loop in thread 'Thread-1'
    # _thread_loop = asyncio.get_event_loop()
    _thread_loop = loop = asyncio.new_event_loop()
    qcmd = asyncio.Queue(loop=loop)
    qdat = asyncio.Queue(loop=loop)
    return loop


def main_loop(server_address, loop=None):
    global active_srv
    active_srv = server_address  # OR another id
    loop = _thread_loop

    conn = core_connect(server_address, loop)
    loop.run_until_complete(conn)
    tasks = [asyncio.ensure_future(qcmd_send(), loop=loop),
             asyncio.ensure_future(qdat_recv(), loop=loop),
             asyncio.ensure_future(qdat_apply(), loop=loop)]
    _all = asyncio.gather(*tasks, loop=loop)
    loop.run_until_complete(_all)
    loop.run_until_complete(core_close())
    loop.close()


def _runner(server_address):
    loop = new_loop()
    is_initialized.release()
    _log.info("Event loop running in thread: {!r}".format(
                threading.current_thread().name))
    try:
        main_loop(server_address, loop)
        pass
    except KeyboardInterrupt:
        pass


# Call from main_thread
def run_in_background(server_address):
    relay = threading.Thread(target=_runner, args=(server_address,))
    # Exit the server thread when the main thread terminates
    relay.daemon = True

    relay.start()
    # Wait until msg bus created
    is_initialized.acquire()
    return relay
