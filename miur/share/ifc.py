import abc
import collections.abc

__all__ = ['ILink']

# ATT: multidirectional dataflow needs two diff ifc
#   * LhsBoundary = Plug : ext -> inn  # receiver : api controls 'src'
#   * RhsBoundary = Slot : inn -> ext  # sender   : api controls 'dst'
# EXPL:
#   * inn ifc is fixed by class spec
#   * inn is always function
#   * ext ifc is specified by class api
#   * ext is always derived Connector object


def _undefined(*p, **k):
    raise NotImplementedError


# NOTE: connector allows chaining
#   self.bind(obj1).bind(obj2) ...
#   self.unbind(obj1).unbind(obj2) ...
#   ++ Can overload __add__ to create chain by: Link1 + Link2 + ...
class Connector(collections.abc.Callable):
    # __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def bind(self, connector):
        """Couple two connectors"""

    @abc.abstractmethod
    def unbind(self, connector=None):
        """Decouple two connectors"""


# ATT: circular dependency of funcs arg types is impossible
# class Slot: def bind(self, f: Plug) && class Plug: def bind(self, f: Slot)

# NOTE: Same src, bind dst
#   * any slot always produces carriers E.G.(wall socket, headphones, hdmi)
#   * slot knows what it will produce
#   * but slot doesn't know where carriers going until it will be connected
#   * slot location is fixed
#     - known to any connected inner generator: __call__() must never be reassigned
#     - known to any plug possible to be inserted outside
class Slot(Connector):
    _plug = _undefined

    def __init__(self, plug: Connector=None):
        self.bind(plug)

    def __call__(self, args):
        return self._plug(args)

    # BAD: unsymmetrical -- must call plug.bind(self) to setup its _slot
    def bind(self, plug: Connector):
        if plug is not self._plug:
            self.unbind()
            if plug is not None:
                assert issubclass(plug, Plug)
                plug.bind(self)
        if plug is not None:
            self._plug = plug
        return plug

    def unbind(self, plug: Connector=None):
        if plug is not None:
            assert plug is self._plug
        self._plug = _undefined
        return plug


# NOTE: Bind src, same dst
#   * any plug always consumes carriers E.G.(two-pin plug, phones jack plug)
#   * slots must always have valid refs: __call__() must never be reassigned
#       => however, if we always unbind() before bind()
class Plug(Connector):
    _slot = _undefined
    _body = _undefined

    def __init__(self, slot: Connector=None, *, body=None):
        self.bind(slot)
        self.set_body(body)

    # THINK: instead of intermediator 'body', can I send to it directly ?
    def set_body(self, body=None):
        self._body = body if body is not None else _undefined

    def __call__(self, args):
        return self._body(args)

    def bind(self, slot: Connector):
        # MAYBE:USE: more fail-safe : don't unbind if slot=None
        if slot is not self._slot:
            self.unbind()
            if slot is not None:
                assert issubclass(slot, Slot)
                slot.bind(self)
        if slot is not None:
            self._slot = slot
        return slot

    # DECIDE: save '_src' to be able to unbind() w/o args OR always demand 'f'
    #   + useful when always doing unbind(f=self._src) before bind, as we need backref
    def unbind(self, slot: Connector=None):
        if slot is not None:
            assert issubclass(slot, Slot)
            slot.unbind(self)
        elif self._slot is not _undefined:
            slot = self._slot
            slot.unbind(self)
        self._slot = _undefined
        return slot


class Link(collections.abc.Callable):
    def __init__(self, src=None, dst=None):
        self._plug = Plug(body=self.__call__)
        self._slot = Slot()
        self.__call__ = self._plug.__call__
        self._sink = self._slot.__call__
        self.bind(src, dst)

    def bind(self, src=None, dst=None):
        # EXPL: backward init order : connect sink before generator
        if dst is not None:
            assert issubclass(dst, Link)
            self._slot.bind(dst.plug)
        if src is not None:
            assert issubclass(src, Link)
            self._plug.bind(src.slot)

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
#   __metaclass__ = abc.ABCMeta
#   @abc.abstractmethod
# SEE
#   https://habrahabr.ru/post/72757/
#   https://zopetoolkit.readthedocs.io/en/latest/
#   https://www.python.org/dev/peps/pep-3119/
#   http://javascriptissexy.com/beautiful-javascript-easily-create-chainable-cascading-methods-for-expressiveness/
