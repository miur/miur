import logging
import asyncio
import functools

from miur.share.chain import Chain
from miur.share.osi import *

from . import command, server

_log = logging.getLogger(__name__)


# NOTE: adapted to bus, works in both dir, aggregates sep concepts of subsystems
class Carrier:
    def __init__(self, *, sink=None, cmd=None, rsp=None, uid=None):
        self.sink = sink
        self.cmd = cmd
        self.rsp = rsp  # THINK: 'rsp' can be 'cmd' if initiated from *core* to *client*
        self.uid = uid  # THINK: unique message id -- remove ?

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


# TODO: negotiate both protocols on connect
#   * negotiate changing protocol at any time (plain_text, srz_dict, etc)
#   * process errors and exceptions
#   * regulate security/access/ability policy
#   * unites recv/send chains over single homogeneous transport
#   * provides STD access point to each client/recipient
class Channel:
    def __init__(self, sink=None, src=None, dst=None, ctx=None):
        make_cmd = command.CommandMaker('miur.core.command.all', ctx).make
        make_car = functools.partial(Carrier, sink=self)
        self.chain_recv = Chain([sink, Deanonymize(make_car),
                                 Deserialize(make_cmd), Desegmentate()],
                                iterator=reversed)
        self.chain_send = Chain([Anonymize(), Serialize(), Segmentate(), dst])
        # HACK: try to insert this into Chain ALT self.get_src_slot()
        src.sink = self.chain_recv
        # BAD: dst isn't closed, ALSO how to close all when disassembling chain
        self.close = src.close
        self.close_recv = src.close_recv

    def __call__(self, car):
        _log.info('Response({:x}): {!r}'.format(car.uid, car.rsp))
        self.chain_send(car)


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
class Handler:
    def __init__(self, sink):
        self.sink = sink

    # MOVE: to the end of self-channel
    # BUT: it's placed in-between in/out bus -- how to simulate imm policy?
    # CHG: use global executor class for this
    # TRY: regulate putting rsp into bus_send by cmd/rsp itself
    #   => BUT who must decide it: command or context ?
    def __call__(self, car):
        car.execute()
        self.sink(car)


# NOTE: this is recv concentrator -- sole point for all channels to connect
#   ~~ BET: rename into Hub, Tie, Knot ?
class Hub(BaseChainLink):
    def __init__(self, sink, handler, ctx):
        self.sink = sink
        self.handler = handler
        # NOTE: 'channels' is sep entity to support multiple independent buses with clients
        # WARN: not thread safe !!! BUT can't prove any until re-impl loop.create_server()
        self.channels = set()
        self.make_channel = functools.partial(Channel, sink=self.recv, ctx=ctx)

    # ALT:BET: transit whole packets stream directly w/o re-encoding
    # if cmd['dst'] != self: self.bus_send.put(car)
    # NOTE:ENH: each blocking/transfer op must block only its own channel, not whole hub!
    #   => MAYBE: impl 'recv()' as part of Channel ?
    def recv(self, car):
        policy = getattr(car.cmd, 'policy', command.GENERAL)
        if policy == command.GENERAL:
            self.sink(car)
        elif policy == command.IMMEDIATE:
            # THINK: place 'shutdown' into qout and wait again or send 'shutdown' immediately from server ?
            #   => must traverse 'shutdown' through whole system to establish proper shutdown chain
            self.handler(car)
        return car

    # RFC: there must not be any recv/send in Hub BUT where them must be ?
    #   => smth aka "Door between Bus and Channel, Gatekeeper"
    def __call__(self, car):
        # DEV: broadcast/multicast
        #   THINK? is it attribute of command or of carrier ?
        # ATT: silent discard of 'car' if dst channel removed
        if car.sink in self.channels:
            car.sink(car)

    # MAYBE: use backward dict [self] = channel :: so I can dismiss self.chan
    # NOTE: adding to dict don't require lock (beside iterating that dict)
    # MAYBE: set() is enough BUT how to close _impl connection then ?
    #   => cascade closing of Channel => BUT then you need: channel._impl_conn = self
    #   BETTER: cascading :: allows to mid-close channel and replace transport
    # BAD: diff args to reg and unreg
    def register(self, conn):
        chan = self.make_channel(src=conn, dst=conn)
        self.channels.add(chan)
        return chan
        # channel.callback(self.put)
        # self.channels[channel.uuid] = channel.get

    def deregister(self, chan):
        self.channels.remove(chan)
        return None
        # del self.channels[channel.uuid]
        # channel.destroy()

    def close_recv(self):
        [chan.close_recv() for chan in self.channels]

    def close(self):
        [chan.close() for chan in self.channels]


# NOTE: topology must aggregate both in/out bus to be able to exec sync cmds
#   THINK:MAYBE: hide both impl in/out queue with exec() under single Bus ?
class Topology:
    def __init__(self, server_address, ctx=None, loop=None):
        self.loop = loop
        # NOTE: separate buses are necessary to support full-duplex
        self.bus_recv = Bus()
        self.bus_send = Bus()
        self.servers = set()
        self.handler = Handler(sink=self.bus_send.put)
        self.hub = Hub(sink=self.bus_recv.put, handler=self.handler, ctx=ctx)

        self.loop.create_task(self.mk_server_coro(TcpListeningServer, server_address))
        self.loop.create_task(self.bus_send.for_each(self.hub))
        self.loop.create_task(self.bus_recv.for_each(self.handler))

    def mk_server_coro(self, factory, server_address):
        srv = factory()
        self.servers.add(srv)
        return srv.start(server_address, hub=self.hub, loop=self.loop)

    def quit_soon(self):
        # EXPL: listening server stops, but already established sockets continue to communicate
        [srv.close() for srv in self.servers]
        # EXPL: immediately ignore all incoming cmds when server is quitting
        self.hub.close_recv()
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
        self.hub.close()

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
