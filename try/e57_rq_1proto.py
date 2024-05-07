# pylint:disable=too-few-public-methods
from abc import abstractmethod
from dataclasses import dataclass
from enum import Enum, auto
from typing import Any, Generic, TypeVar, override, Protocol

T = TypeVar("T")


class ObjectChangeKind(Enum):
    # pylint:disable=invalid-name
    insert = auto()
    remove = auto()
    change = auto()  # RENAME? .replace .assign .modify
    # move/swap (=chg xpath)
    # incr/decr (included in "change" together with .assign)


# T=(int,slice[int],list[int]|str,tuple[str],list[str] || Predicate|Filter)
@dataclass(frozen=True, kw_only=True)
class ObjectChangeNotification(Generic[T]):
    ts: int  # USE: time.time_ns()
    kind: ObjectChangeKind
    xpath: T  # NOTE: interpreted by dst `*ContainerStructure class
    newval: Any | None
    oldval: Any | None
    object: Any | None  # optional backref to full origin (parent) object


# RENAME: Notifiable(notify) | Subscriptable(process) Listening(accept)
class Notifiable(Protocol):
    def notify(self, msg: ObjectChangeNotification) -> None: ...


# ALT(proto/enum) -> Filter/ProtocolObject(spec)
class OCRequestKind(Enum):
    # pylint:disable=invalid-name
    none = auto()
    full = auto()
    incr = auto()
    # auto = ...  # MAYBE: to auto-pick full/incr whatever supported (by processing methods presence)


class PropagationMixin:
    _subs: dict[Notifiable, bool]

    # RENAME? .sync_to() OR .link_to() OR .announce_to
    def sub(self, obj: Notifiable, proto: OCRequestKind) -> None:
        if not hasattr(self, "_subs"):
            self._subs = {}
        self._subs[obj] = proto

    def unsub(self, obj: Notifiable) -> None:
        # WARN: don't ignore silently if obj not in dict
        del self._subs[obj]
        if not self._subs:
            del self._subs

    def _publish(self) -> None:
        if not hasattr(self, "_subs"):
            return
        for obj, proto in self._subs.items():
            match proto:
                case OCRequestKind.none:
                    continue
                case OCRequestKind.full:
                    obj.notify(self)
                case OCRequestKind.incr:
                    obj.notify_incr(self)
                case _:
                    raise NotImplementedError(proto)


class ResultMixin(Generic[T]):
    _result: T

    @property
    def result(self) -> T:
        if not hasattr(self, "_result"):
            raise ValueError("you should call .update() beforehand")
        return self._result


# RENAME: WatchingComputation(Notifiable)
# ALSO: IncrementalComputation, AdaptiveComputation, IterativeComputation
class Computation(ResultMixin[T], Notifiable, PropagationMixin):
    @abstractmethod
    def _body(self) -> T: ...

    def _refresh(self) -> None:
        self._result = self._body()
        self._publish()

    # WIP
    # RENAME? __call__ eval calc update
    # MOVE(update) -> `CachedMixin
    def update(self) -> T:
        # if not self._ready:  # TBD:API: .invalidate() | .ready()
        self._refresh()
        return self._result  # XPMT: do we need this sugar ?

    def notify(self, _msg: ObjectChangeNotification) -> None:
        # TODO: assert msg.kind == ObjectChangeKind.change
        self._refresh()


# RENAME? same as PostulatingComputation
class Value(Computation[T]):
    def __init__(self, value: T) -> None:
        self._result = value

    @override
    def update(self) -> T:
        return self._result

    @override
    def _body(self) -> T:
        pass


# RENAME? CachedVariablePropagation
#   ALT: .value inof .result (SEP class from `Computation)
class Variable(Computation[T]):
    def __init__(self, value: T) -> None:
        self.update(value)

    @override
    def update(self, value: T, /) -> T:
        self._result = value
        self._publish()
        return self._result

    @override
    def _body(self) -> T:
        pass


# RENAME: SomeChainedCalculation
class Plus3(Computation[int]):
    def __init__(self, comp: Computation) -> None:
        self._comp = comp

    @override
    def _body(self) -> int:
        return self._comp.result + 3


def _live() -> None:
    ### Pull
    v2 = Value(2)
    f3 = Plus3(v2)
    f3.update()
    print(f3.result)
    print(Plus3(Value(2)).update())

    ### Push
    ## deps_graph
    w1 = Variable(1)
    c1 = Plus3(w1)
    c2 = Plus3(c1)
    ## conn_graph
    w1.sub(c1, OCRequestKind.full)
    c1.sub(c2, OCRequestKind.full)
    ## vflow_graph
    w1.update(5)  # NOTE: change value and trigger Propagation
    print(c2.result)
