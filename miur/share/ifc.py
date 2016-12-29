import logging
import abc
from collections.abc import Callable, Iterable

__all__ = ['ILink']

_log = logging.getLogger(__name__.split('.', 2)[1])

# ATT: multidirectional dataflow needs two diff ifc
#   * LhsBoundary = Plug : ext -> inn  # receiver : api controls 'src'
#   * RhsBoundary = Slot : inn -> ext  # sender   : api controls 'dst'
# EXPL:
#   * inn ifc is fixed by class spec
#   * inn is always function
#   * ext ifc is specified by class api
#   * ext is always derived Boundary object

# http://stackoverflow.com/questions/14648374/python-function-calls-are-really-slow
#   indirect calling :: ~100nS per call => Chain() may become slow (ALSO whole callback topology)


def _undefined(*p, **k):
    raise NotImplementedError


# ATT: circular dependency of funcs arg types is impossible
# class Slot: def bind(self, f: Plug) && class Plug: def bind(self, f: Slot)
# ALT:(name): couple/decouple
class IBind(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def bind(self, outer):
        raise NotImplementedError

    @abc.abstractmethod
    def unbind(self, outer=None):
        raise NotImplementedError

    # ALT: hasbound
    @abc.abstractmethod
    def isbound(self, outer=True):
        raise NotImplementedError


class IJoint:
    @abc.abstractproperty
    def plug(self):
        """Read Input"""

    @abc.abstractproperty
    def slot(self):
        """Write Output"""


# NOTE: connector allows chaining
#   self.bind(obj1).bind(obj2) ...
#   self.unbind(obj1).unbind(obj2) ...
#   ++ Can overload __add__ to create chain by: Link1 + Link2 + ...
# ALT:(name): Flow
class Boundary(Callable, IBind):
    _inner = _undefined
    _outer = _undefined

    # MAYBE:USE: more fail-safe : don't unbind if outer=None
    # WARN: w/o 'mutual' bind() is called triple instead of twice (+race cond)
    # WARN: doesn't gurantee mutual bind()
    #   = if outer is self._outer: return outer
    #   = if self._outer is _undefined: return outer
    def bind(self, outer: IBind, mutual=True):
        self.unbind()
        if outer is not None:
            assert callable(outer)
            # EXPL: moved under 'mutual' to allow in Chain homogeneous connects
            if mutual and isinstance(outer, self._outer_t):
                outer.bind(self, False)
            self._outer = outer
        return outer

    def unbind(self, outer: IBind=None, mutual=True):
        if outer is None and self._outer is not _undefined:
            outer = self._outer
        if outer is not None:
            assert callable(outer)
            if mutual and isinstance(outer, self._outer_t):
                outer.unbind(self, False)
        self._outer = _undefined
        return outer

    def isbound(self, outer=True):
        if isinstance(outer, bool):
            return outer == (self._outer != _undefined)
        elif isinstance(outer, IBind):
            return outer == self._outer


# NOTE: Same src, bind dst
#   * any slot always produces carriers E.G.(wall socket, headphones, hdmi)
#   * slot knows what it will produce
#   * but slot doesn't know where carriers going until it will be connected
#   * slot location is fixed
#     - known to any connected inner generator: __call__() must never be reassigned
#     - known to any plug possible to be inserted outside
# ALT:(name): OutFlow, OutsideFlow
class Slot(Boundary):
    def __init__(self, outer=None):
        self._outer_t = Plug  # HACK: circular deps :: no declaration in Python
        self.bind(outer)

    # THINK: how it can be used ? MAYBE in chain for {chain[-1].slot--slot} conn ?
    def set_inner(self, inner=None):
        if inner is not None:
            assert isinstance(inner, self._outer_t)
            inner.set_inner(self)

    def __call__(self, *args, **kw):
        return self._outer(*args, **kw)


# NOTE: Bind src, same dst
#   * any plug always consumes carriers E.G.(two-pin plug, phones jack plug)
#   * slots must always have valid refs: __call__() must never be reassigned
#       => however, if we always unbind() before bind() -- you can ?
# ALT:(name): InFlow, InsideFlow
class Plug(Boundary):
    def __init__(self, outer=None, inner=None):
        self._outer_t = Slot  # HACK: circular deps :: no declaration in Python
        self.bind(outer)
        self.set_inner(inner)

    def set_inner(self, inner=None):
        self._inner = inner if inner is not None else _undefined

    def __call__(self, *args, **kw):
        return self._inner(*args, **kw)


class IConnector(IBind, IJoint):
    def __init__(self, src=None, dst=None):
        self.bind(src, dst)

    def bind(self, src=None, dst=None):
        # EXPL: backward init order : dangling producer on exc is safer then consumer
        if dst is not None:
            _log.info('Bind ({!r}) -> {!r}'.format(self.slot, dst))
            assert isinstance(dst, IConnector)
            self.slot.bind(dst.plug)
        if src is not None:
            _log.info('Bind {!r} -> ({!r})'.format(src, self.plug))
            assert isinstance(src, IConnector)
            self.plug.bind(src.slot)

    def unbind(self, src=None, dst=None):
        if src is not None:
            assert isinstance(src, IConnector)
            self.plug.unbind(src.slot)
        else:
            self.plug.unbind()
        if dst is not None:
            assert isinstance(dst, IConnector)
            self.slot.unbind(dst.plug)
        else:
            self.slot.unbind()

    def isbound(self, plug=True, slot=True):
        return self.plug.isbound(plug) and self.slot.isbound(slot)


# USE:(C++): private inheritance to hide impl (members _sink/_plug)
# ALT:(name): LinkedFlow
class Link(IConnector):
    def __init__(self, src=None, dst=None, inner=None):
        self._slot = Slot()
        self._plug = Plug(inner=(inner if inner is not None else self._slot))
        super().__init__(src=src, dst=dst)

    @property
    def plug(self):
        return self._plug

    @property
    def slot(self):
        return self._slot

    # THINK: Is there need for __call__ ?
    def __call__(self, *args, **kw):
        return self._slot(*args, **kw)


# USE: sink/well, giver/taker, producer/consumer
# WTF: no visible diff with 'Link' -- TRY using in Channel/Hub to understand
# ALT:(name) Duplex/Latch/Lock/Clamp/Frame/Chassis/IO
# ALT:(name): BidirectFlow
class Socket(IConnector):
    def __init__(self, src=None, dst=None, inner=None):
        self._slot = Slot()
        self._plug = Plug(inner=(inner if inner is not None else self._slot))
        super().__init__(src=src, dst=dst)

    @property
    def plug(self):
        return self._plug

    @property
    def slot(self):
        return self._slot


# ALT:(name): ChainedFlow
# BAD: slight discrepancy for isbound() using plug/slot instead of bet/end links
class Chain(IConnector):
    def __init__(self, chain: Iterable=None, *, src=None, dst=None):
        self.chain = chain
        # THINK: excessive specifying of args
        self._beg = Link(src=src)
        self._end = Link(dst=dst)
        super().__init__(src=src, dst=dst)
        self.invalidate()

    @property
    def plug(self):
        return self._beg.plug

    @property
    def slot(self):
        return self._end.slot

    def __call__(self, *args, **kw):
        fn = self.plug
        return fn(*args, **kw)

    @property
    def chain(self):
        return self._chain

    @chain.setter
    def chain(self, chain):
        if chain is None:
            self._chain = []
        else:
            for c in chain:
                assert isinstance(c, IConnector)
            self._chain = chain
        return self._chain

    # ENH: split in three to update only necessary parts
    def invalidate(self):
        src = self._beg
        for link in self._chain:
            src.bind(dst=link)
            src = link
        src.bind(dst=self._end)


# ALT:(name): Cord/Cabel/Socket
class Channel(IBind):
    def __init__(self, lhs=None, rhs=None):
        self._lhs = Socket()
        self._rhs = Socket()
        self._l2r = Chain(src=self._lhs, dst=self._rhs)
        self._r2l = Chain(src=self._rhs, dst=self._lhs)
        self.bind(lhs=lhs, rhs=rhs)

    @property
    def lhs(self):
        return self._lhs

    @property
    def rhs(self):
        return self._rhs

    # THINK: isallowed() -- encapsulate assert isinstance(..., Socket)
    # TRY: encapsulate whole patt 'bind() if not None'
    def bind(self, lhs=None, rhs=None):
        if lhs is not None:
            assert isinstance(lhs, Socket)
            self.lhs.bind(src=lhs.slot, dst=lhs.plug)
        if rhs is not None:
            assert isinstance(rhs, Socket)
            self.rhs.bind(src=rhs.slot, dst=rhs.plug)

    # def unbind(self, lhs=None, rhs=None):
    #     if lhs is not None:
    #         assert isinstance(lhs, Socket)
    #         self.plug.unbind(lhs.slot)
    #     else:
    #         self.lhs.unbind(lhs)
    #         self.rhs.unbind(rhs)
    #     if rhs is not None:
    #         assert isinstance(rhs, IConnector)
    #         self.slot.unbind(rhs.plug)
    #     else:
    #         self.slot.unbind()

    # def isbound(self, lhs=True, rhs=True):
    #     return self.lhs.isbound(lhs) and self.rhs.isbound(rhs)


# NOTE: half-hub == mux/demux are useful on their own -- like parts of Handler
class Hub(IBind):
    def __init__(self, sink, handler, ctx):
        # THINK: allow heterogeneous conn circuits
        #   OR:BET: allow only 'Socket' conn and create special 'adapter' for heterogeneous ones
        self._mux = None    # ALT:(name): multiplug = track all plugs to disconnect at once
        self._demux = None  # ALT:(name): multislot

    # NOTE: heterogeneous left and right sides
    def bind(self, bus=None, sock=None):
        pass

    # HACK: we access rhs, create new Socket object per connection and return it to bind() with Channel
    #   BUT: if bind wasn't successful :: how to remove from container and destroy this object ?
    @property
    def conn(self):
        sock = Socket()
        self._channels.add(sock)
        return sock


# NOTE: return passed obj for chaining : self.bind(obj1).bind(obj2)
# WARN:(link.__call__) impossible to reuse _sink like 'chain.bind(self._sink)'
#   ++ ALSO I can use simple functions as _sink
# Unidirectional
# ALT:(name): ISimplex, slot/plug, sink; push/pull for deffered bus = queue + coro
class ILink:
    def __new__(cls, *p, **k):
        # CHECK:WTF: when ILink used as mix-in ?
        obj = object.__new__(cls)
        obj._sink = cls.__sink
        return obj

    @staticmethod
    def __sink(*p, **k):
        """Private ref to next link"""
        raise NotImplementedError

    def bind(self, link, *, mutual=False):
        """Bind next link to this"""
        # THINK? better order when exception
        # THINK? where mutual linking may be useful ? Only short-circuit ?
        if mutual:
            link.bind(self)
        self.unbind(link)
        self._sink = link
        return link

    def unbind(self, link, *, mutual=False):
        """Safe unbind to override by dispatchers"""
        if mutual:
            link.unbind(self)
        if self._sink != self.__sink:
            assert self._sink == link
            # CHECK: if this reassign works as expected
            self._sink = self.__sink
        return link

    def __call__(self, *args, **kwargs):
        """Push processed data into next link"""
        # ALT:FIND: directly assign self.__call__ = self._sink
        self._sink(*args, **kwargs)


# USE: impl abstract ifc based on 'abc' OR look at more formalized way of 'zope'
#   import abc
# SEE
#   https://habrahabr.ru/post/72757/
#   https://zopetoolkit.readthedocs.io/en/latest/
#   https://www.python.org/dev/peps/pep-3119/
#   http://javascriptissexy.com/beautiful-javascript-easily-create-chainable-cascading-methods-for-expressiveness/
