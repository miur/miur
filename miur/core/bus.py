import logging
import asyncio

from miur.share import protocol

from . import command

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
    def __init__(self, make_cmd, ctx=None):
        self.make_cmd = make_cmd  # factory
        self.ctx = ctx
        self.qin = asyncio.Queue()
        self.qout = asyncio.Queue()

    def put_cmd(self, dst, data_whole):
        obj, ifmt = protocol.deserialize(data_whole)
        _log.debug('Packet({!r}b): {!r}'.format(len(data_whole), obj))
        cmd = self.make_cmd(obj['cmd'], self.ctx, *obj['args'])
        car = Carrier(dst, cmd, fmt=ifmt, uid=obj['id'])
        # BETTER: instead 'car' resend directly incoming packet inside transport
        #   => Lesser delay loop and no cpu load on re-encoding, better streaming
        # if cmd['addressee'] != self:  # transit
        #     self.qout.put_nowait(car)
        # else:
        self._push(car)
        return car

    def _push(self, car):
        policy = getattr(car.cmd, 'policy', command.GENERAL)
        if policy == command.GENERAL:
            self.qin.put_nowait(car)
        elif policy == command.IMMEDIATE:
            self.execute(car)

    # MOVE: to the end of self-channel
    # BUT: it's placed in-between in/out bus -- how to simulate imm policy?
    def execute(self, car):
        rsp = car.execute()
        self.qout.put_nowait(rsp)

    # THINK? all_conn must be aggregated by Bus class or rsp_dispatcher coro ?
    def pop_rsp(self, car, all_conn):
        rsp = {'id': car.uid, 'rsp': car.rsp}
        data = protocol.serialize(rsp, car.fmt)
        _log.info('Response({:x}): {!r}'.format(car.uid, rsp['rsp']))
        return all_conn[car.dst].send(data)


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


class TcpTransport(BaseTransport):
    def recv_cb(self, obj):
        pass

    def send(self, obj):
        pass


# NOTE: protocol negotiated between *mods*(uuid) on each *mods* topology change
class Channel:
    def __init__(self):
        self.transport = None
        self.protocol = None  # negotiated on connection, DFL depends on conn type
        self.recv_cb = None  # transport1.recv
        self.send = None  # transport2.send

    def set_transport(self, reader, writer=None):
        pass

    def recv_cb(self, obj):
        pass

    def send(self, obj):
        self.protocol.send(obj)


# NOTE: hub must aggregate both in/out bus to be able to exec sync cmds
class Hub:
    def __init__(self, loop):
        self.loop = loop
        # NOTE: separate buses are necessary to support full-duplex
        self.bus_recv = None
        self.bus_send = None
        self.channels = {}

    def register(self, channel):
        channel.callback(self.put)
        self.channels[channel.uuid] = channel.get

    def deregister(self, channel):
        del self.channels[channel.uuid]
        channel.destroy()
