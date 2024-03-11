import os
from abc import ABCMeta
from pathlib import Path
from typing import Any, Iterable

from typing_extensions import ParamSpec

P = ParamSpec("P")

# pylint:disable=too-few-public-methods


class Entity(metaclass=ABCMeta):
    default = "actionlist"


class ActionEntity(Entity):
    default = "execute"


class InodeEntity(Entity):
    def __init__(self, path: str | Path) -> None:
        self._path = Path(path)


class FileEntity(InodeEntity):
    def get_text(self) -> str:
        with open(self._path, "r", encoding="utf-8") as f:
            return f.readline()


class DirEntity(InodeEntity):
    # def get_actions(self) -> Iterable[ActionEntity]:
    #     yield ShowActionEntity("FSListWidget", "FSView", "get_listing")
    #     yield ShowActionEntity("VirtualListWidget", "EntityView", "get_actions")
    #     pass

    # BET?(Any): use proper methods to typecheck in/out args inof type-erasure
    # def __getitem__(self, k: str) -> Any: if k == "name": return ...
    def name(self) -> str:
        return self._path.as_posix()

    def __iter__(self) -> Iterable[ActionEntity]:
        yield LsinodesAction(self)

    def __getitem__(self, k: str) -> ActionEntity:
        for act in self:  # BAD:PERF
            if act.name == "LsInodes":
                return act
        raise NotImplementedError


# FIXED:RENAME: DirView -> AsyncListingView
#   * we shouldn't cache both DirEntity and generic Listing with STD list-trfm API in single DirView
class AsyncListingView:
    def __init__(self, act: LsinodesAction) -> None:
        self._act = act
        self.sorted = lambda x: x.name
        self.reversed = False

    def __iter__(self) -> Iterable[Entity]:
        # OR: xs = self._act() where _act=parent
        #   MAYBE: parent for DirListing *is* DirAction, and not DirEntity itself ?
        #     ~~ depends on «chain compression», or «action erasure»
        xs = self._act()
        if self.sorted:
            xs = sorted(xs, key=self.sorted, reverse=self.reversed)
        elif self.reversed:
            xs = reversed(xs)
        return xs


class LsinodesAction(ActionEntity):
    def __init__(self, ent: DirEntity) -> None:
        self._ent = ent
        self.name = 'LsInodes'

    def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> AsyncListingView[InodeEntity]:
        with os.scandir(self._ent._path) as it:
            # THINK: construct new `*Entity directly here (to include ctx)
            #        or keep str() until later API -- to reduce memory consumption
            # for e in it:
            #     yield (DirEntity if x.is_dir() else FileEntity)(e.path)
            return [e.name for e in it]


class PrinterDevice:
    def add_line(self, x: int, y: int, s: str) -> None:
        print("  " * x + str(y) + ": " + s)


# RENAME? Widget -> Presenter/Adapter
class DirWidget:
    _ent: DirEntity
    _lst: AsyncListingView

    def __init__(self) -> None:
        self._show = "LsInodes"  # = «default action name»

    def set_entity(self, ent: DirEntity) -> None:
        # WARN: should probably be DirCachedProxy to cache fetched and generated results
        self._ent = ent
        self._lst = self._ent[self._show]()

    def render_to(self, odev: PrinterDevice) -> None:
        odev.add_line(0, 0, self._ent.name)
        for i, ent in enumerate(self._lst, start=1):
            odev.add_line(1, i, ent)

    def handle_keypress(self, key: str) -> None:
        if key == "l":  # vim-right (to open directory)
            # TODO: switch to next Entity (to test reassign on Action.exec())
            print(key)
        elif key == "o":  # =ordering
            self._lst.reversed = not self._lst.reversed
            print(key)
        else:
            raise NotImplementedError


def _live() -> None:
    dtmp = Path("/t")  # =/tmp/testmiur
    dtmp.mkdir(exist_ok=True)
    for c in "abc":
        dtmp.joinpath(c).touch(exist_ok=True)

    ent = DirEntity("/t")
    view = DirView(ent)
    wdg = DirWidget()
    wdg.set_view(view)

    odev = PrinterDevice()
    wdg.render_to(odev)

    # wdg.handle_keypress("o")
    # wdg.render_to(odev)
    # wdg.handle_keypress("l")
    # wdg.render_to(odev)
