import curses as C
import sys
from typing import Any

from .tui import TUI


def navi(**_kw: Any) -> Any:
    return main()


def main() -> None:
    if not C.has_extended_color_support():
        raise NotImplementedError
    # C.initscr()
    # C.start_color()
    # FIXME: must reassign stdout only temporarily until curses binds itself to TTY
    with TUI() as tui:
        scr = tui.stdscr

        scr.clear()
        i: int
        for i, x in enumerate(sys.stdin):
            scr.addstr(i, 0, f"{i}: {x}")

        fg = 217
        bg = 17
        # info = C.color_content(fg)
        info = C.COLOR_PAIRS
        pair = 30
        C.init_pair(pair, fg, bg)

        scr.addstr(i + 1, 0, str(info), C.color_pair(pair))
        scr.refresh()
        scr.getkey()
        # C.napms(1500)
