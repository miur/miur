# import os
from abc import ABCMeta  # , abstractmethod
from typing import Callable, Generic, Iterable, Protocol, Self, TypeVar, TYPE_CHECKING

from typing_extensions import ParamSpec

if not TYPE_CHECKING:
    reveal_type = print

T = TypeVar("T")
R = TypeVar("R")
P = ParamSpec("P")

# pylint:disable=too-few-public-methods

# Generic[T]
# class ImplementsPointed(Protocol):
#     def __init__(self, x: T) -> None:
#         ...
#
# class ImplementsFunctor(Protocol):
#     def fmap(self, fn: Callable[[T]]) -> T:
#         ...
#
# class Entity(metaclass=ABCMeta):
#     @abstractmethod
#     def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> Iterable["Entity"]: ...
#
# # RENAME? "Named|BeingNamed"
# class HasName(Protocol):
#     @property
#     # @abstractmethod
#     def name(self) -> str:
#         ...
#         # cls = type(self).__name__.removesuffix("Entry")  # "Action"
#         # return f"{cls}({repr(self._x)})"


# RENAME?= Fractal, Junction, Branching, Composable, Discoverable, Unwrappable, Expandable
#   BAD: I want to use "Entity" everywhere in SRC, not some other word
class CallComposable(Generic[T]):  # , metaclass=ABCMeta
    def __init__(self, x: T) -> None:
        self._x = x

    def __call__(self, fsp: Callable[[T], "CallComposable[R]"]) -> "CallComposable[R]":
        return fsp(self._x)

    # def __call__(self, fsp: Callable[[T], "Entity[R]"]) -> "Entity[R]":
    #     return fsp(self._x)
    #
    # NOTE "python-returns" makes it @abstractmethod COS Monads are different
    #   SEE: how it actually accomplished (T->R) return type in mypy
    #     /d/research/miur/functional/returns/returns
    # _Self = TypeVar("_Self", bound="Entity[T]")
    # def fmap(self: _Self, fn: Callable[[T], R]) -> _Self:
    #     return type(self)(fn(self._x))


class Entity(CallComposable[T]):
    pass


class Action(Entity, metaclass=ABCMeta):
    pass
    # @abstractmethod
    # def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> Iterable[Entity]:
    #     ...


class Entry(Entity, metaclass=ABCMeta):
    pass

    # @abstractmethod
    # def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> Iterable[Action]:
    #     ...


# class ListInodesAction(Action):
#     # [_] TODO:DECI: shouldn't I pass "Entity" inof internal value?
#     #   << NEED give direct access to internal «Monadic» value Ma->f(a)->Mb
#     def __init__(self, path: str) -> None:
#         self._path = path
#     # [_] THINK:DECI: feature-based vs identity-based
#     #   ~ full IMPL here (tight locality), totally opaque FSEntity with no props
#     #   ~ shallow binding here (thin spread), FSEntity provides low-level property
#     def __call__(self) -> Iterable[FSEntry]:
#         with os.scandir(self._path) as it:
#             return [FSEntry(e.path) for e in it]


# NOTE: same as Monadic {Ma->f(x)->Mb}
class ListInodesAction(Action):
    def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> Iterable[FSEntry]:
        return [FSEntry("/t")]
        # with os.scandir(path) as it:
        #     return [FSEntry(e.path) for e in it]


class FSEntry(Entry):
    def __init__(self, path: str) -> None:
        self._path = path

    # FAIL: dif signature, dif return type
    #   [_] THINK: what can we do here?
    #     ? pack all args into sep class?  OR:(same):USE "Any" ?
    def __call__(self) -> Iterable[Action]:
        return [ListInodesAction()]


class PrinterDevice:
    def add_line(self, x: int, y: int, s: str) -> None:
        print("  " * x + str(y) + ": " + s)


class DirWidget:
    _ent: Entity
    _lst: list[Entity]

    def __init__(self, ent: FSEntry) -> None:
        self.set_entity(ent)

    def set_entity(self, ent: Entity) -> None:
        self._ent = ent
        self._lst = list(ent())

    def __getitem__(self, idx: int) -> Entity:  # TEMP:API
        return self._lst[idx]

    def render_to(self, odev: PrinterDevice) -> None:
        odev.add_line(0, 0, self._ent.name)
        for i, ent in enumerate(self._lst, start=1):
            odev.add_line(1, i, ent.name)


def _live() -> None:
    wdg = DirWidget(FSEntry("/etc/udev"))
    odev = PrinterDevice()
    wdg.render_to(odev)
    wdg.set_entity(wdg[0])
    wdg.render_to(odev)
