import curses as C
from typing import Self, assert_never

from ..systems.tuisystem import DisplayList, TextSpan


# SEP/OPT::
#   * fullscreen (vs embedded piece-of-screen)
#   * curses (vs my own native-tui) (vs blessed)
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
        # FIXME: for embedding we may want to return whatever was set during .resize(...)
        #   MAYBE: also return wx,wy for embedding offsets, or whole `Rect/`CellRect at once
        h, w = self.stdscr.getmaxyx()
        return w, h

    def draw_lines(self, lines: list[str]) -> None:  # CHG? bytes
        self.stdscr.clear()
        for s in lines:
            self.stdscr.addstr(s)
        self.stdscr.refresh()

    def draw_displ(self, displ: DisplayList) -> None:
        self.stdscr.clear()
        for token in displ:
            match token:
                case TextSpan(x, y, text, wc, sid):
                    # MAYBE: sanitize string out of interfering /\n|\r/
                    # FIXME: shortly exit fn on resize to avoid curses crash
                    #   WHY: no sense to crop frame on shrink or continue drawing on enlarge,
                    #     as displ should be recalculated for adaptive-layout anyway
                    # FIXME: convert my styleid to curses fg/bg/attr-id
                    self.stdscr.addnstr(y, x, text, wc, sid)
                case _:
                    assert_never(token)
        self.stdscr.refresh()
