# pylint:disable=too-few-public-methods
from abc import abstractmethod
from typing import Any, Generic, TypeVar, override, Protocol

T = TypeVar("T")


# RENAME: Notifiable(notify) | Subscriptable(process) Listening(accept)
class Notifiable(Protocol):
    # FUT: only accept Notification() msgs
    def notify(self, msg: Any) -> None: ...


class PropagationMixin:
    _subs: dict[Notifiable, bool]

    # CHG(proto) -> enum/ProtocolObject(spec)
    def sub(self, obj: Notifiable, proto: bool = True) -> None:
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
            if proto is True:
                obj.notify(self)


# RENAME: CachedVariablePropagation
#   !! same as PostulatingComputation
# class Variable(Generic[T]):
#     def __init__(self, x: T) -> None:
#         self._x = x
#
#     @property
#     def value(self) -> T:
#         return self._x


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

    def notify(self, _msg: Any) -> None:
        self._refresh()


class Value(Computation[T]):
    def __init__(self, value: T) -> None:
        self._result = value

    @override
    def update(self) -> T:
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
    v1 = Value(1)
    c1 = Plus3(v1)
    c2 = Plus3(c1)
    ## conn_graph
    v1.sub(c1)
    c1.sub(c2)
    ## vflow_graph
    # TODO: Variable.update(5) to change value and trigger Propagation
    v1._publish()
    print(c2.result)
