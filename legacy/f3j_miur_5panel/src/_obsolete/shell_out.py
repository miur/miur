import os
import sys
from contextlib import contextmanager
from subprocess import CompletedProcess, run
from threading import BoundedSemaphore
from typing import Any, AnyStr, Callable, Iterator, Sequence, TextIO

import _curses as C


## FAIL: clears bkgr, moves cursor, +/- doesn't show old buffer
# clear = "\033[H"
# mainscr = "\033[?1049l"
# altscr = "\033[?1049h"
# print(mainscr + "mylogline" + altscr, end="", file=__import__("sys").stderr)
## ALT:
# tput = lambda s: tui.otty.write(C.tigetstr(s).decode(tui.otty.encoding))
# tput("rmcup")
# print(C.LINES)
# tput("smcup")
# @contextmanager
# def curses_altscreen(stdscr: C.window) -> Iterator[None]:
#     """NICE: redirect all logs to primary altscreen"""
#     C.def_prog_mode()  # save current tty modes
#     C.endwin()  # restore original tty modes
#     try:
#         yield
#     finally:
#         stdscr.refresh()  # restore save modes, repaint screen


@contextmanager
def stdio_to_altscreen(stdscr: C.window, ttyio: TextIO) -> Iterator[None]:
    oldwrite = ttyio.write

    # WARN:PERF: switching back-n-forth this way -- takes 0.5ms on each logline
    # ALT:HACK: fflush=oldw.__self__.flush
    def _write(s: AnyStr) -> int:
        with curses_altscreen(stdscr, fflush=ttyio.flush):
            return oldwrite(s)

    ttyio.write = _write  # type:ignore[assignment]
    try:
        yield
    finally:
        ttyio.write = oldwrite  # type:ignore[method-assign]
