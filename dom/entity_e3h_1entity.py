import os
from typing import Iterable, Self

# pylint:disable=too-few-public-methods


class FSEntity:
    def __init__(self, path: str) -> None:
        self._path = path

    @property
    def name(self) -> str:
        return self._path

    def __call__(self) -> Iterable[Self]:
        with os.scandir(self._path) as it:
            return [type(self)(e.path) for e in it]


class PrinterDevice:
    def add_line(self, x: int, y: int, s: str) -> None:
        print("  " * x + str(y) + ": " + s)


class DirWidget:
    _ent: FSEntity
    _lst: list[FSEntity]

    def __init__(self, ent: FSEntity) -> None:
        self.set_entity(ent)

    def set_entity(self, ent: FSEntity) -> None:
        self._ent = ent
        self._lst = list(ent())

    def __getitem__(self, idx: int) -> FSEntity:  # TEMP:API
        return self._lst[idx]

    def render_to(self, odev: PrinterDevice) -> None:
        odev.add_line(0, 0, self._ent.name)
        for i, ent in enumerate(self._lst, start=1):
            odev.add_line(1, i, ent.name)


def _live() -> None:
    wdg = DirWidget(FSEntity("/etc/udev"))
    odev = PrinterDevice()
    wdg.render_to(odev)
    wdg.set_entity(wdg[1])
    wdg.render_to(odev)
