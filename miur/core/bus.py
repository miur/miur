import logging
import asyncio
import functools
import struct

from miur.share import protocol

from . import command, server

_log = logging.getLogger(__name__)


# NOTE: adapted to bus, works in both dir, aggregates sep concepts of subsystems
class Carrier:
    def __init__(self, dst=None, cmd=None, rsp=None, fmt=None, uid=None):
        self.dst = dst  # pair (sid, cid) -- listening server + accepted client
        self.cmd = cmd
        self.rsp = rsp  # THINK: 'rsp' can be 'cmd' if initiated from *core* to *client*
        self.fmt = fmt  # MAYBE: excessive and must be removed
        self.uid = uid  # unique message id

    # WTF if cmd will be executed second time ?
    #   THINK: Allow or disable availability
    def execute(self, *args):
        _log.debug('Command: {!r}'.format(self.cmd.cmd))
        self.rsp = self.cmd.execute(*args)
        return self


# NOTE: there is sense in constructing cmds directly on receiving
#   => so I can immediately construct QuitMsg and then decide when to execute it
#       (immediately or after rest of queue -- to support quit_now and graceful_exit)
#   =>> then executor() will be only calling '.execute()' method and qout.put()

# FIXME: design how to catch/distribute global ctx like self.loop to cmds ?
# MAYBE: pass self.loop as main()->CommandMaker and there pass it to each cmds on __init__

# NOTE: bus encapsulates cmds caching and ordering (priority, queue, pool, etc)
class Bus:
    def __init__(self):
        self._impl = asyncio.Queue()
        self.running = True

    def put(self, obj):
        # NOTE: raises QueueFull if can't put it now
        return self._impl.put_nowait(obj)

    async def join(self):
        await self._impl.join()
        self.running = False

    async def pop_apply(self, functor):
        car = await self._impl.get()
        functor(car)
        self._impl.task_done()

    # BETTER: replace with 'async for' instead of 'while True' and move to Bus
    #   https://www.python.org/dev/peps/pep-0492/#asynchronous-iterators-and-async-for
    # TRY:(queues.py:164) try: [g.cancel() for g in _impl._getters]; except QueueEmpty: pass
    async def for_each(self, functor):
        while self.running:
            await self.pop_apply(functor)


class BaseTransport:
    def recv_cb(self, obj):
        raise NotImplementedError

    def send(self, obj):
        raise NotImplementedError


# NOTE: loopback transport to inject cmds in backward direction
class DirectTransport(BaseTransport):
    def recv_cb(self, obj):
        return obj

    # XXX: what it must return ?
    def send(self, obj):
        self.recv_cb(obj)


# RFC: pack/unpack will be the same for all streams/pipes/fifo
# BUT: for UDP unpack requires sending requests for repeating lost UDP packets
# DEV: periodically send 'heartbeat' data and drop incomplete msg on each
#   heartbeat, raising error or requesting msg re-send from client
# MAYBE protocol is exactly this functionality for merging segments and heartbeat ?
#     => then curr code in Protocol must be moved into Presentation
class TcpTransport(BaseTransport):
    h_sz_len = 4

    def __init__(self, recv_cb=None, conn=None):
        self.recv_cb = recv_cb
        self.conn = conn

        self._n = self.h_sz_len
        self._buf = bytearray()
        self._head = True

    # BAD: broken chain of ret= value when accumulating
    #   ~? ret 'None' when consumed and 'car' when fully converted
    def recv(self, data):
        # Accumulate data even if too small for both branches
        self._buf = self._parse_msg(self._buf + data)

    # BAD: packet dst set by the last chunk of data -- can be tampered up
    def _parse_msg(self, buf):
        i = 0
        # NOTE: used single cycle to process multiple msgs received at once
        while (i + self._n) <= len(buf):
            blob = buf[i:i + self._n]
            i += self._n
            if self._head:
                self._n = struct.unpack('>I', blob)[0]
            else:
                self._n = self.h_sz_len
                _log.debug('_n({!r}b)'.format(self._n))
                _log.debug('blob({!r}b)'.format(len(blob)))
                self.recv_cb(blob)
            self._head = not self._head
        return buf[i:]

    def send(self, packet):
        header = struct.pack('>I', len(packet))
        data = header + packet
        return self.conn.send(data)

    def close(self):
        self.conn.close()

    def close_recv(self):
        self.conn.close_recv()


class BaseProtocol:
    def pack(self):
        raise NotImplementedError

    def unpack(self):
        raise NotImplementedError


# NOTE: can split single cmd into multiple msgs -- for streaming, etc
class DictProtocol:
    def __init__(self, make_cmd, make_car):
        self.make_cmd = make_cmd
        self.make_car = make_car

    # RFC: remove intermediate 'dict' (if possible)
    def unpack(self, packet):
        obj, _ = protocol.deserialize(packet)
        _log.debug('Packet({!r}b): {!r}'.format(len(packet), obj))
        cmd = self.make_cmd(obj['cmd'], *obj['args'])
        car = self.make_car(cmd=cmd, uid=obj['id'])
        return car

    # RFC: remove redundant 'ifmt/ofmt'
    def pack(self, car):
        _log.info('Response({:x}): {!r}'.format(car.uid, car.rsp))
        rsp = {'id': car.uid, 'rsp': car.rsp}
        packet = protocol.serialize(rsp, car.fmt)
        return packet


# NOTE: protocol negotiated between *mods*(uuid) on each *mods* topology change
#   * supports different transport for recv and send
# DEV: main dst key is ref to 'Channel' (direct pointer or new random uuid)
#   * uuid is associated with channel only after recipient exchange first msg (according to protocol)
#   * any consequent other uuid from same channel -- also registered and associated
#     => if transit recipient relocates between nodes, all rsp sent to its old
#       dst until it send at least one req from its new location
#     ENH: if node says us that transit recipient is absent we can postpone rsp until it appears smwr again
# BAD: channel identified by uuid can be tampered
#   !! any recipient can forge cmd which rsp will be sent to another recipient (unexpecting that rsp!)

# TODO:ENH: make Channel contain two Chain, where Chain ~= 'compose()'
# BAD: 'compose()' -- freezes order :: BUT chain must be reconfigured when topology changes
# + https://docs.python.org/3.1/howto/functional.html
#   http://stackoverflow.com/questions/16739290/composing-functions-in-python
#   https://mathieularose.com/function-composition-in-python/
# ALSO: Channel manages end-points to connect/replace buses and transports
class Channel:
    def __init__(self, recv_cb=None, conn=None, ctx=None):
        self.recv_cb = recv_cb
        # BAD:THINK:RFC: unsymmetrical passing of recv and send
        self.transport = TcpTransport(recv_cb=self.recv, conn=conn)
        _make_cmd = command.CommandMaker('miur.core.command.all').make  # factory
        make_car = functools.partial(Carrier, dst=self, fmt=protocol.pickle)

        def make_cmd(nm, *args):
            return _make_cmd(nm, ctx, *args)
        # negotiated on connection, DFL depends on conn type
        self.protocol = DictProtocol(make_cmd, make_car)

    def recv(self, packet):
        obj = self.protocol.unpack(packet)
        return self.recv_cb(obj)

    def send(self, obj):
        packet = self.protocol.pack(obj)
        return self.transport.send(packet)

    def close(self):
        self.transport.close()

    def close_recv(self):
        self.transport.close_recv()


class TcpListeningServer:
    # BETTER: re-impl loop.create_server(...) for deterministic order of accept/receive/lost
    #   => then parallelism will be controllable and no need for _lock in channels
    async def start(self, server_address, channels, make_channel, loop):
        self._asyncio_srv = await loop.create_server(
            functools.partial(server.ClientProtocol, channels, make_channel),
            *server_address, reuse_address=True, reuse_port=True)
        peer = self._asyncio_srv.sockets[0].getsockname()
        _log.info('Serving on {}'.format(peer))

    def close(self):
        return self._asyncio_srv.close()

    async def wait_closed(self):
        return await self._asyncio_srv.wait_closed()


# NOTE:TEMP: mix up 'load distributor' thread with 'executor' thread while N==1
class Handler:
    def __init__(self, sink):
        self.sink = sink

    # MOVE: to the end of self-channel
    # BUT: it's placed in-between in/out bus -- how to simulate imm policy?
    # CHG: use global executor class for this
    # TRY: regulate putting rsp into bus_send by cmd/rsp itself
    #   => BUT who must decide it: command or context ?
    def handle(self, car):
        rsp = car.execute()
        self.sink(rsp)


# NOTE: hub must aggregate both in/out bus to be able to exec sync cmds
#   THINK:MAYBE: hide both impl in/out queue with exec() under single Bus ?
class Hub:
    def __init__(self, server_address, ctx=None, loop=None):
        self.loop = loop
        # NOTE: separate buses are necessary to support full-duplex
        self.bus_recv = Bus()
        self.bus_send = Bus()
        # NOTE: 'channels' is sep entity to support multiple independent buses with clients
        # WARN: not thread safe !!! BUT can't prove any until re-impl loop.create_server()
        self.channels = set()
        self.servers = set()
        self.handler = Handler(self.bus_send.put)

        self.loop.create_task(self.mk_server_coro(TcpListeningServer, server_address, ctx))
        self.loop.create_task(self.bus_send.for_each(self.send))
        self.loop.create_task(self.bus_recv.for_each(self.handler.handle))

    def mk_server_coro(self, factory, server_address, ctx):
        srv = factory()
        # XXX! i can set Channel.send_cb only after connection established !!!
        make_channel = functools.partial(Channel, recv_cb=self.recv, ctx=ctx)
        coro = srv.start(server_address, self.channels, make_channel, loop=self.loop)
        self.servers.add(srv)
        return coro

    # NOTE: this is recv concentrator -- sole point for all channels to connect
    #   ~~ ? BET: rename into Hub ?
    # BETTER: instead 'car' resend directly incoming packet inside transport
    #   => Lesser delay loop and no cpu load on re-encoding, better streaming
    # if cmd['addressee'] != self:  # transit
    #     self.qout.put_nowait(car)
    def recv(self, car):
        policy = getattr(car.cmd, 'policy', command.GENERAL)
        if policy == command.GENERAL:
            self.bus_recv.put(car)
        elif policy == command.IMMEDIATE:
            # THINK: place 'shutdown' into qout and wait again or send 'shutdown' immediately from server ?
            #   => must traverse 'shutdown' through whole system to establish proper shutdown chain
            self.handler.handle(car)
        return car

    # RFC: there must not be any recv/send in Hub BUT where them must be ?
    #   => smth aka "Door between Bus and Channel, Gatekeeper"
    def send(self, car):
        # DEV: broadcast/multicast
        #   THINK? is it attribute of command or of carrier ?
        # ATT: silent discard of 'car' if dst channel removed
        if car.dst in self.channels:
            return car.dst.send(car)
            # return self.channels[car.dst].send(car)

    def quit_soon(self):
        # EXPL: listening server stops, but already established sockets continue to communicate
        [srv.close() for srv in self.servers]
        # EXPL: immediately ignore all incoming cmds when server is quitting
        [chan.close_recv() for chan in self.channels]
        self.loop.stop()  # EXPL: break main loop and await quit_clean() only

    # WARN! must be the last task !  No more async coro after this !
    async def quit_clean(self):
        await asyncio.gather(*[srv.wait_closed() for srv in self.servers])
        # NOTE: qin is already exhausted -- shutdown message is always the last
        # one -- and it triggers closing of executor()
        await self.bus_recv.join()
        # BAD! wait qout only after all executors done!
        #   => qin.pop() is immediate, but qout.put() is often delayed until cmd finished
        # TRY: use semaphore with timer ?
        await self.bus_send.join()
        [chan.close() for chan in self.channels]

    # RFC: unused == try using instead of direct assing inside ClientProtocol
    def register(self, channel):
        channel.callback(self.put)
        self.channels[channel.uuid] = channel.get

    def deregister(self, channel):
        del self.channels[channel.uuid]
        channel.destroy()

# TODO:
#   MOVE: rename channel hierarchy
#     TcpProtocol->CommandPresentation
#     TcpTransport->StreamProtocol
#     ClientProtocol->TcpTransport
#   ENH: self talk with bus by its own reverse channel
#     ADD loopback channel for msg addressed to itself
#       -- until it will be forked out from bus
#   THINK:RFC: eliminate ref to channel in each carrier
#       => ref is more lightweight but breaks encapsulation and makes system more coupled
#   CHECK: closing recv end
#   DEV: dynamic add/remove servers by cmds
#   NEED: verify whole channel path till transport => fix encapsulation on ends
#   FIXME: synchronous execution
