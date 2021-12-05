import curses as C
import sys
from typing import Any

from .tui import TUI


def navi(**_kw: Any) -> Any:
    return main()


def main() -> None:
    with TUI() as tui:
        scr = tui.stdscr
        scr.clear()
        for i, x in enumerate(sys.stdin):
            scr.addstr(i, 0, f"{i}: {x}")
        scr.refresh()
        scr.getkey()
        # C.napms(1500)
