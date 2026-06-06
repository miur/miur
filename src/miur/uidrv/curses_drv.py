import curses as C
from typing import Self

from ..kernel import MiurKernel, NaviId
from ..systems.tuisystem import DisplayList, VisibleArea


class CursesUIDriver:
    def __init__(self) -> None:
        self.displ: DisplayList
        self.lines: list[str]  # CHG? bytes
        self.stdscr: C.window
        self._pvis: int

    def __enter__(self) -> Self:
        if not C.has_extended_color_support():
            raise NotImplementedError
        C.setupterm()
        self.stdscr = C.initscr()
        C.noecho()
        C.raw()
        self.stdscr.keypad(True)
        self._pvis = C.curs_set(0)
        C.start_color()
        self.stdscr.nodelay(True)
        return self

    def __exit__(self, *_a: object) -> None:
        self.stdscr.refresh()
        self.stdscr.nodelay(False)
        C.curs_set(self._pvis)
        self.stdscr.keypad(False)
        C.echo()
        C.noraw()
        C.endwin()

    def bake(self, kernel: MiurKernel, nvid: NaviId, va: VisibleArea) -> None:
        va.wnd_h, va.wnd_w = self.stdscr.getmaxyx()
        va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, va.wnd_h)
        self.displ, self.lines = kernel.navi_sequence(nvid, va)

    def draw(self) -> None:
        self.stdscr.clear()
        for s in self.lines:
            self.stdscr.addstr(s)
        self.stdscr.refresh()
