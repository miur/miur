import shutil
from typing import Self

from ..kernel import MiurKernel, NaviId
from ..systems.tuisystem import DisplayList, VisibleArea


class PrintTextUIDriver:
    def __init__(self) -> None:
        self.displ: DisplayList = []
        self.lines: list[str] = []  # CHG? bytes
        self.print = print

    def __enter__(self) -> Self:
        return self

    def __exit__(self, *_a: object) -> None:
        pass

    def bake(self, kernel: MiurKernel, nvid: NaviId, va: VisibleArea) -> None:
        va.wnd_w, va.wnd_h = shutil.get_terminal_size(fallback=(80, 24))
        va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, va.wnd_h)
        self.displ, self.lines = kernel.navi_sequence(nvid, va)

    def draw(self) -> None:
        self.print("".join(self.lines))
