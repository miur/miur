import os
from abc import ABCMeta
from itertools import islice
from pathlib import Path
from typing import Any, Iterable, Self

# pylint:disable=too-few-public-methods


class Entity(metaclass=ABCMeta):
    default = "actionlist"


class ActionEntity(Entity):
    default = "execute"


class InodeEntity(Entity):
    def __init__(self, path: str | Path) -> None:
        self._path = Path(path)


class DirEntity(InodeEntity):
    default = "inodes"

    # def get_inodes(self) -> Iterable[Self | FileEntity]:
    #     for x in os.scandir(self._path):
    #         if x.is_dir():
    #             yield DirEntity(x.path)
    #         else:
    #             yield FileEntity(x.path)
    #
    # def get_actions(self) -> Iterable[ActionEntity]:
    #     yield ShowActionEntity("FSListWidget", "FSView", "get_listing")
    #     yield ShowActionEntity("VirtualListWidget", "EntityView", "get_actions")
    #     pass

    def __getitem__(self, k: str) -> Any:
        if k == "name":
            return self._path.as_posix()
        if k == "inodes":
            return map(str, self._path.iterdir())
        raise NotImplementedError


class FileEntity(InodeEntity):
    default = "lines"

    def get_text(self) -> str:
        with open(self._path, "r", encoding="utf-8") as f:
            return f.readline()


class DirView:
    def __init__(self, ent: DirEntity) -> None:
        self._ent = ent

    def __getitem__(self, k: str) -> Any:
        return self._ent[k]


class PrinterDevice:
    def add_line(self, x: int, y: int, s: str) -> None:
        print("  " * x + str(y) + ": " + s)


class DirWidget:
    _view: DirView

    def set_view(self, view: DirView) -> None:
        self._view = view

    def render_to(self, odev: PrinterDevice) -> None:
        odev.add_line(0, 0, self._view["name"])
        for i, ent in enumerate(self._view["inodes"], start=1):
            odev.add_line(1, i, ent)

    def handle_keypress(self, key: str) -> None:
        if key == "s":
            # TODO: switch to next Entity (to test reassign on Action.exec())
            print(key)
        raise NotImplementedError


def _live() -> None:
    for c in "abc":
        Path("/t").joinpath(c).touch(exist_ok=True)

    ent = DirEntity("/t")
    view = DirView(ent)
    wdg = DirWidget()
    wdg.set_view(view)

    odev = PrinterDevice()
    wdg.render_to(odev)

    wdg.handle_keypress("s")
    wdg.render_to(odev)
