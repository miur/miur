import curses as C
import os
import sys
from typing import Any, BinaryIO

import _curses

STDIOS = (sys.stdin, sys.stdout, sys.stderr)


def get_ttyname() -> str:
    return next(os.ttyname(f.fileno()) for f in STDIOS if f.isatty())


# [_] CMP: /@/research/airy/pacman/pikaur/pikaur/pikspect.py:87: class TTYInputWrapper():
# stdout - How to I make python curses application pipeline friendly? - Stack Overflow ⌇⡡⢬⡺⠄
#   https://stackoverflow.com/questions/53696818/how-to-i-make-python-curses-application-pipeline-friendly
# percol/percol at master · mooz/percol ⌇⡡⢬⡹⣵
#   https://github.com/mooz/percol/tree/master/percol
def rebind_stdios_to_tty(tty: Any) -> dict[str, int]:
    desc = {}
    for f in STDIOS:
        nm = f.name[1:-1]
        desc[nm] = os.dup(f.fileno())
        os.dup2(tty.fileno(), f.fileno())
        try:
            # HACK: override high-level objects by dups and abandon low-level fileno
            setattr(sys, nm, os.fdopen(desc[nm], f.mode))
        except OSError:  # wrong mode or /dev/null
            # desc[nm] = None
            raise
    return desc


class TUI:
    def __init__(self) -> None:
        self.stdscr: C.window | None = None
        self.tty: BinaryIO | None = None
        self.old: dict[str, int] | None = None

    def __enter__(self) -> "TUI":
        self.tty = open(get_ttyname(), "wb+", buffering=0)
        self.old = rebind_stdios_to_tty(self.tty)

        C.setupterm(term=os.environ.get("TERM", "unknown"), fd=sys.__stdout__.fileno())

        self.stdscr = _curses.initscr()
        for key, value in _curses.__dict__.items():
            if key[0:4] == "ACS_" or key in ("LINES", "COLS"):
                setattr(C, key, value)

        C.noecho()  # echoing of keys = off
        C.cbreak()  # buffering on keyboard input = off
        self.stdscr.keypad(True)  # sup special escape seq for e.g. curses.KEY_LEFT
        try:
            C.start_color()  # ignorable
        except:  # pylint:disable=bare-except
            pass

        return self

    def __exit__(self, exc_type: Any, exc_value: Any, traceback: Any) -> None:
        print("clean")
        if self.stdscr is not None:
            self.stdscr.keypad(False)
            C.echo()
            C.nocbreak()
            C.endwin()
        if self.tty is not None:
            self.tty.close()
