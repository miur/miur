import abc
from collections.abc import Callable

__all__ = ['ILink']

# ATT: multidirectional dataflow needs two diff ifc
#   * LhsBoundary = Plug : ext -> inn  # receiver : api controls 'src'
#   * RhsBoundary = Slot : inn -> ext  # sender   : api controls 'dst'
# EXPL:
#   * inn ifc is fixed by class spec
#   * inn is always function
#   * ext ifc is specified by class api
#   * ext is always derived Boundary object


def _undefined(*p, **k):
    raise NotImplementedError


# ATT: circular dependency of funcs arg types is impossible
# class Slot: def bind(self, f: Plug) && class Plug: def bind(self, f: Slot)
class IBind:
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def bind(self, outer):
        """Couple two connectors"""

    @abc.abstractmethod
    def unbind(self, outer=None):
        """Decouple two connectors"""


# NOTE: connector allows chaining
#   self.bind(obj1).bind(obj2) ...
#   self.unbind(obj1).unbind(obj2) ...
#   ++ Can overload __add__ to create chain by: Link1 + Link2 + ...
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
            if mutual:
                # EXPL: moved under 'mutual' to allow in Chain homogeneous connects
                assert isinstance(outer, self._outer_t)
                outer.bind(self, False)
            self._outer = outer
        return outer

    def unbind(self, outer: IBind=None, mutual=True):
        if outer is None and self._outer is not _undefined:
            outer = self._outer
        if outer is not None:
            if mutual:
                assert isinstance(outer, self._outer_t)
                outer.unbind(self, False)
        self._outer = _undefined
        return outer


# NOTE: Same src, bind dst
#   * any slot always produces carriers E.G.(wall socket, headphones, hdmi)
#   * slot knows what it will produce
#   * but slot doesn't know where carriers going until it will be connected
#   * slot location is fixed
#     - known to any connected inner generator: __call__() must never be reassigned
#     - known to any plug possible to be inserted outside
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
class Plug(Boundary):
    def __init__(self, outer=None, inner=None):
        self._outer_t = Slot  # HACK: circular deps :: no declaration in Python
        self.bind(outer)
        self.set_inner(inner)

    def set_inner(self, inner=None):
        self._inner = inner if inner is not None else _undefined

    def __call__(self, *args, **kw):
        return self._inner(*args, **kw)


class Link(Callable):
    def __init__(self, src=None, dst=None):
        self._slot = Slot()
        self._plug = Plug(inner=self._slot)
        self.bind(src, dst)

    def bind(self, src=None, dst=None):
        # EXPL: backward init order : dangling producer on exc is safer then consumer
        if dst is not None:
            assert isinstance(dst, self.__class__)
            self._slot.bind(dst.plug)
        if src is not None:
            assert isinstance(src, self.__class__)
            self._plug.bind(src.slot)

    def unbind(self, src=None, dst=None):
        if src is not None:
            assert isinstance(src, self.__class__)
            self._plug.unbind(src.slot)
        if dst is not None:
            assert isinstance(dst, self.__class__)
            self._slot.unbind(dst.plug)

    @property
    def plug(self):
        return self._plug

    @property
    def slot(self):
        return self._slot


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
