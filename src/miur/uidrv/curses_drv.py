import curses as C
from typing import Self


class CursesUIDriver:
    def __init__(self) -> None:

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

    def sizewh(self) -> tuple[int, int]:
        h, w = self.stdscr.getmaxyx()
        return w, h

    def draw(self, lines: list[str]) -> None:  # CHG? bytes
        self.stdscr.clear()
        for s in lines:
            self.stdscr.addstr(s)
        self.stdscr.refresh()
