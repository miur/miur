from abc import ABCMeta, abstractmethod
from typing import (
    TYPE_CHECKING,
    Annotated,
    Any,
    Callable,
    Generic,
    Iterable,
    Iterator,
    Protocol,
    Self,
    Sequence,
    TypeVar,
)

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


############################################################
_Tx_co = TypeVar("_Tx_co", covariant=True)
_Tx_contra = TypeVar("_Tx_contra", contravariant=True)

class Applicative(Protocol[_Tx_contra]):
    def __call__(self, ent: Composable[_Tx_contra], /) -> Composable[Any]: ...


_actions: dict[type, list["ActionM[Any, Any]"]] = {}


# Action = Entity[Context] + Applicative[Entity] + Semi-Lifted[SpecialFunction]
class ActionM(Entity[_Tx_co], Applicative[_Tx_contra]):
    def __new__(cls) -> Self:
        obj = super().__new__(cls)
        # ta = next(v for k,v in get_type_hints(obj.__call__).items())
        # from typing import get_type_hints
        # import inspect
        # inspect.signature(obj)
        # _actions.setdefault(ta, []).append(obj)
        # list(inspect.signature(ListFSEntries.__call__).parameters.values())[1].annotation
        return obj

    def __call__(self, ent: Composable[_Tx_contra], /) -> Entity[Any]:
        return ent * self._sfn

    # _sfn: Applicable[_Tx_contra]
    # BAD: can't rename args in derived class, unless I use trailing "/"
    @abstractmethod
    def _sfn(self, x: _Tx_contra, /) -> Entity[Any]: ...


class ListFSEntriesM(ActionM[None, str]):
    # FAIL: we can't specialize signature in derived classes
    def __call__(self, ent: FSEntry, /) -> CachedListing[FSEntry]:
        return ent * self._sfn

    def _sfn(self, path: str, /) -> CachedListing[FSEntry]:
        with __import__("os").scandir(path) as it:
            return CachedListing(FSEntry(e.path) for e in it)


def _live2() -> None:
    en0 = FSEntry("/etc/udev")
    ac1 = ListFSEntriesM(None)
    ac2 = GetByIndex(1)
    # FIXME: ac* should inherit from ActionM for this to work
    _en1: FSEntry = ac2(ac1(en0))


# RENAME? List[All|Available|Supported|Implemented|Accepted]Actions
class ListActions(ActionM[None, type]):
    def __call__(
        self, ent: Composable[_Tx_contra], /
    ) -> CachedListing[ActionM[None, None]]:
        return self._sfn(type(ent))

    def _sfn(self, cls: type, /) -> CachedListing[ActionM[None, None]]:
        return CachedListing(cls.get_insts())
        # * list bound to FSEntity -- FAIL: recursive types, use independent db
        # * list bound to all super(FSEntity) -- USE: __mro__ | .mro() | inspect.getmro()
        # * list all Protocol compatible with FSEntity
        # * list all compatible with Inner value type of FSEntity
