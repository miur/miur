from abc import ABCMeta, abstractmethod
from typing import TYPE_CHECKING, Callable, Generic, Protocol, TypeVar

# pylint:disable=too-few-public-methods
if not TYPE_CHECKING:
    reveal_type = print

T = TypeVar("T")
U = TypeVar("U")

# from typing_extensions import ParamSpec
# P = ParamSpec("P")


# RENAME? "Named|HasName|BeingNamed"
class Named(Protocol):
    @property
    @abstractmethod
    def name(self) -> str:
        """Return unique identifier under class context e.g. filename in folder"""
        # cls = type(self).__name__.removesuffix("Entry")  # "Action"
        # return f"{cls}({repr(self._x)})"


class ImplementsFunctor(Protocol):
    def fmap(self, fn: Callable[[T], T]) -> T: ...

    # NOTE "python-returns" makes it @abstractmethod COS Monads are differing
    #   SEE: how it actually accomplished (T->R) return type in mypy
    #     /d/research/miur/functional/returns/returns
    # _Self = TypeVar("_Self", bound="Entity[T]")
    # def fmap(self: _Self, fn: Callable[[T], R]) -> _Self:
    #     return type(self)(fn(self._x))


# RENAME?= Fractal, Junction, Branching, Composable, Discoverable, Unwrappable, Expandable
#   BAD: I want to use "Entity" everywhere in SRC, not some other word
# RENAME?= ChainedByCall
class CallComposable(Generic[T]):
    _x: T

    def __call__(self, fsp: Callable[[T], "CallComposable[U]"]) -> "CallComposable[U]":
        return fsp(self._x)


# ALT:
# class MulComposable(Protocol[T_co]):
#     def __mul__(self, fsp: Callable[[T_co], "Entity[U]"]) -> "Entity[U]": ...


# [_] THINK:DECI: feature-based vs identity-based
#   ~ full IMPL here (tight locality), totally opaque FSEntity with no props
#   ~ shallow binding here (thin spread), FSEntity provides low-level property
class Action(CallComposable[T], metaclass=ABCMeta):
    ## FAIL: single action may produce different `*Entries at once
    def __init__(self, m: type) -> None:
        self._m = m

    # @abstractmethod
    # def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> Iterable[Entity]:
    #     ...
