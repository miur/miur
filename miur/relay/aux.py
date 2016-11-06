import logging
import socket
import asyncio

from miur.relay import protocol


_log = logging.getLogger(__name__)
servers = {}  # task -> (reader, writer)


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
#   ? Can preview be treated as such ?

def client_done(task):
    del servers[task]
    _log.info("Client Task Finished")
    # if len(servers) == 0:
    #     _log.info("servers is empty, stopping loop.")
    #     loop = asyncio.get_event_loop()
    #     loop.stop()


def make_connection(server_address, loop):
    task = loop.create_task(handle_client(server_address))
    servers[task] = server_address
    _log.info("New Client Task")
    task.add_done_callback(client_done)
    return task


async def handle_client(server_address):
    reader, writer = await asyncio.open_connection(*server_address)
    _log.info("Connected to %s %d", *server_address)

    msg = 'yes\n'
    writer.write(msg.encode('utf-8'))
    await writer.drain()  # MAYBE: need only before 'close()' ?

    data = await asyncio.wait_for(reader.readline(), timeout=2.0)
    print(data)

    writer.close()


def loop(server_address):
    loop = asyncio.get_event_loop()
    coro = make_connection(server_address, loop)
    loop.run_until_complete(coro)
    loop.close()
