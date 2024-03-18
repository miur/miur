import os
from abc import ABCMeta, abstractmethod
from typing import Iterable

from typing_extensions import ParamSpec

P = ParamSpec("P")

# pylint:disable=too-few-public-methods


# RENAME?= Fractal, Junction, Branching, Composable, Discoverable, Unwrappable, Expandable
#   BAD: I want to use "Entity" everywhere in SRC, not some other word
class Entity(metaclass=ABCMeta):
    @property
    @abstractmethod
    def name(self) -> str:
        ...

    @abstractmethod
    def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> Iterable["Entity"]:
        ...


class Entry(Entity, metaclass=ABCMeta):
    @property
    def name(self) -> str:
        cls = self.__class__.__name__.removesuffix("Entry")
        # TODO? should instead access internal value e.g. _path
        return f"{cls}({self})"

    @abstractmethod
    def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> Iterable[Action]:
        ...


class Action(Entity, metaclass=ABCMeta):
    @property
    def name(self) -> str:
        return self.__class__.__name__.removesuffix("Action")

    @abstractmethod
    def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> Iterable[Entry]:
        ...


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
    def __call__(self, path: str) -> Iterable[FSEntry]:
        with os.scandir(path) as it:
            return [FSEntry(e.path) for e in it]


class FSEntry(Entry):
    def __init__(self, path: str) -> None:
        self._path = path

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
