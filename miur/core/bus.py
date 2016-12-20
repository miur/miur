import logging
import asyncio
import functools

from miur.share.chain import Chain
from miur.share import ifc
from miur.share.osi import *

from . import command, server

_log = logging.getLogger(__name__.split('.', 2)[1])


# NOTE: adapted to bus, works in both dir, aggregates sep concepts of subsystems
class Carrier:
    def __init__(self, *, sink=None, cmd=None, rsp=None, uid=None):
        # THINK:RFC: eliminate 'sink' ref to channel in each carrier
        #   => ref is more lightweight but breaks encapsulation and makes system more coupled
        self.sink = sink
        self.cmd = cmd
        self.rsp = rsp  # THINK: 'rsp' can be 'cmd' if initiated from *core* to *client*
        self.uid = uid  # THINK: unique message id -- remove ?

    # WTF if cmd will be executed second time ?
    #   THINK: Allow or disable availability
    def execute(self, sink, *args):
        _log.debug('Command: {!r}'.format(self.cmd.cmd))
        self.rsp = self.cmd.execute(*args)
        # IDEA: 'sink' placed here to support ILink ifc in any cmd and allow data streaming
        return sink(self)  # OR? return self


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


# TODO: negotiate both protocols on connect
#   * negotiate changing protocol at any time (plain_text, srz_dict, etc)
#   * process errors and exceptions
#   * regulate security/access/ability policy
#   * unites recv/send chains over single homogeneous transport
#   * provides STD access point to each client/recipient
# NEED: verify whole channel path till transport => fix encapsulation on ends
# NOTE: Channel == deferred Chain :: cmd goes in call(), transmutates in another prg and returns to sink()
# ??? Do I need '.drain()' before reconfiguring chain to check or push up all
#   -- partial data stored in intermediate state of Links going to be detached ???

# SUM: for symmetrical connection of channel based on callbacks, I need to have
#   ILink ifc on both sides {Hub <=> Channel <=> Connection}

# NOTE: Channel with plugged-in Transport works as deferred Chain with ILink ifc
#   => MAYBE create sep entity encapsulating such ifc ?

# ALT:(name): circuit
class Channel:
    # ALT:(name): src/emitter/producer, dst/collector/absorber
    def __init__(self, sink=None, lhs=None, rhs=None, ctx=None):
        make_cmd = command.CommandMaker('miur.core.command.all', ctx).make
        make_car = functools.partial(Carrier, sink=self)

        # HACK: try insert 'src' into Chain as first and 'dst' as last
        #   => ++ I will get unified ifc: [0] to access 'src' and [-1] to access 'dst'
        self.chain_to_left = Chain()  # producer=src, sink=sink
        self.chain_to_left[:] = [Desegmentate(), Deserialize(make_cmd), Deanonymize(make_car)]
        self.chain_to_right = Chain()  # producer=self, sink=dst
        self.chain_to_right[:] = [Anonymize(), Serialize(), Segmentate()]
        self.plug(lhs=lhs, rhs=rhs)

        # self.__call__ = self.chain_to_right
        # BAD: dst isn't closed, ALSO how to close all when disassembling chain
        # BAD: no support for heterogeneous chains
        self.close = rhs.close
        self.close_recv = rhs.close_recv

    def plug(self, lhs=None, rhs=None):
        if lhs:
            self.plug_lhs(both=lhs)
        if rhs:
            self.plug_rhs(both=rhs)

    def plug_lhs(self, *args, **kw):
        return self._plug(self.chain_to_left, self.chain_to_right, *args, **kw)

    def plug_rhs(self, *args, **kw):
        return self._plug(self.chain_to_right, self.chain_to_left, *args, **kw)

    # recv/send, call/sink
    def _plug(self, producer, consumer, both=None, *, src=None, dst=None):
        if both and not src:
            src = both
        if both and not dst:
            dst = both
        if src:
            src.bind(consumer)
            # TODO: insert into super().bind() to be always called
            _log.info('Bind {}'.format(consumer))
        if dst:
            producer.bind(dst)


class TcpListeningServer:
    # BETTER: re-impl loop.create_server(...) for deterministic order of accept/receive/lost
    #   => then parallelism will be controllable and no need for _lock in channels
    async def start(self, server_address, hub, loop):
        self._asyncio_srv = await loop.create_server(
            functools.partial(server.ClientProtocol, hub),
            *server_address, reuse_address=True, reuse_port=True)
        peer = self._asyncio_srv.sockets[0].getsockname()
        _log.info('Serving on {}'.format(peer))

    def close(self):
        return self._asyncio_srv.close()

    async def wait_closed(self):
        return await self._asyncio_srv.wait_closed()


# NOTE:TEMP: mix up 'load distributor' thread with 'executor' thread while N==1
class Handler(ifc.Link):
    def __init__(self, sink):
        self.sink = sink

    # MOVE: to the end of self-channel
    # BUT: it's placed in-between in/out bus -- how to simulate imm policy?
    # CHG: use global executor class for this
    # TRY: regulate putting rsp into bus_send by cmd/rsp itself
    #   => BUT who must decide it: command or context ?
    def __call__(self, car):
        car.execute(self.slot)


# NOTE: this is recv concentrator -- sole point for all channels to connect
#   ~~ BET: rename into Hub, Tie, Knot ?
# FIXME: hub not manages channels itself -- only registers them for dispatching
#   => so self.channels != all_channels but only links to already registered ones
# THINK: deny calling by ILink API from not registered channel which
#   -- anyway bears callback link to Hub inside its .sink()
#   -- likewise disconnected but not destroyed channel.
# ALT:(name): Mux, Dispatcher, Relay
class Hub(ifc.Link):
    def __init__(self, sink, handler, ctx):
        self._sink = sink
        self.handler = handler
        # NOTE: 'channels' is sep entity to support multiple independent buses with clients
        # WARN: not thread safe !!! BUT can't prove any until re-impl loop.create_server()
        self.channels = set()
        self.make_channel = functools.partial(Channel, lhs=self, ctx=ctx)

    # MAYBE: use backward dict [self] = channel :: so I can dismiss self.chan
    # NOTE: adding to dict don't require lock (beside iterating that dict)
    # MAYBE: set() is enough BUT how to close _impl connection then ?
    #   => cascade closing of Channel => BUT then you need: channel._impl_conn = self
    #   BETTER: cascading :: allows to mid-close channel and replace transport
    # ALT:(name): ++ attach/detach, ~~ connect/disconnect
    def bind(self, chan: Channel):
        super().bind(chan)
        self.channels.add(chan)
        # channel.callback(self.put)
        # self.channels[channel.uuid] = channel.get

    def unbind(self, chan):
        self.channels.remove(chan)
        super().unbind(chan)
        # del self.channels[channel.uuid]
        # channel.destroy()

    # FIXME: synchronous execution
    # ALT:BET: transit whole packets stream directly w/o re-encoding
    # if cmd['dst'] != self: self.bus_send.put(car)
    # NOTE:ENH: each blocking/transfer op must block only its own channel, not whole hub!
    #   => MAYBE: impl 'recv()' as part of Channel ?
    #   + by posting cmds to Hub through chan_self I can imm transfer/multicast them to their dst
    def __call__(self, car):
        policy = getattr(car.cmd, 'policy', command.GENERAL)
        if policy == command.GENERAL:
            self._sink(car)
        elif policy == command.IMMEDIATE:
            # THINK: place 'shutdown' into qout and wait again or send 'shutdown' immediately from server ?
            #   => must traverse 'shutdown' through whole system to establish proper shutdown chain
            self.handler(car)
        return car

    # RFC: there must not be any recv/send in Hub BUT where them must be ?
    #   => smth aka "Door between Bus and Channel, Gatekeeper"
    def dispatch(self, car):
        # DEV: broadcast/multicast
        #   THINK? is it attribute of command or of carrier ?
        # ATT: silent discard of 'car' if dst channel removed
        if car.sink in self.channels:
            car.sink(car)

    def close_recv(self):
        [chan.close_recv() for chan in self.channels]

    def close(self):
        [chan.close() for chan in self.channels]


# NOTE: topology must aggregate both in/out bus to be able to exec sync cmds
#   THINK:MAYBE: hide both impl in/out queue with exec() under single Bus ?
# NOTE: actually whole ring is == Channel() BUT: deferred and with active coro
# TODO: together both buses of Ring can be separated into DeferredChannel which Chains based on Queue
class Ring:
    def __init__(self, ctx):
        # NOTE: separate buses are necessary to support full-duplex
        self.bus_recv = Bus()
        self.bus_send = Bus()
        self.handler = Handler(sink=self.bus_send.put)
        self.hub = Hub(sink=self.bus_recv.put, handler=self.handler, ctx=ctx)

    async def loop(self):
        await asyncio.gather(self.bus_recv.for_each(self.handler),
                             self.bus_send.for_each(self.hub.dispatch))

    def close_recv(self):
        # EXPL: immediately ignore all incoming cmds when server is quitting
        self.hub.close_recv()

    async def quit_clean(self):
        # NOTE: qin is already exhausted -- shutdown message is always the last
        # one -- and it triggers closing of executor()
        await self.bus_recv.join()
        # BAD! wait qout only after all executors done!
        #   => qin.pop() is immediate, but qout.put() is often delayed until cmd finished
        # TRY: use semaphore with timer ?
        await self.bus_send.join()
        self.hub.close()


class Topology:
    def __init__(self, server_address, ctx=None, loop=None):
        self.loop = loop
        self.ring = Ring(ctx=ctx)
        self.loop.create_task(self.ring.loop())

        # DEV: dynamic add/remove servers by cmds
        self.servers = set()
        srv = TcpListeningServer()
        self.servers.add(srv)
        self.loop.create_task(srv.start(server_address, hub=self.ring.hub, loop=self.loop))

        # ENH: self talk with bus by its own reverse channel
        # * Loopback chan must be added in general way through loopback listening server
        #   ADD loopback channel for msg addressed to itself
        #     -- until it will be forked out from bus
        #     = all cmds posted to 'self' will rotate through Ring and rsp go back into chan_self

        # ALT:(name): loopback
        # ALSO:FIX: self Channel must have another set of ChainLinks (=empty)
        #   TEMP: maybe both DFL chain factory are supposed to be embedded into
        #   -- listening servers and passed through key 'factory=(f_recv, f_send)'
        # self.chan_self = Channel(sink=self.recv, ctx=ctx, src=conn, dst=conn)
        # self.ring.hub.register(self.chan_self)

    def quit_soon(self):
        # EXPL: listening server stops, but already established sockets continue to communicate
        [srv.close() for srv in self.servers]
        self.ring.close_recv()
        self.loop.stop()  # EXPL: break main loop and await quit_clean() only

    # WARN! must be the last task !  No more async coro after this !
    async def quit_clean(self):
        await asyncio.gather(*[srv.wait_closed() for srv in self.servers])
        await self.ring.quit_clean()
