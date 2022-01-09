import curses as C
import io
import os
import sys
from typing import Any, TextIO

import _curses


def get_ttyname() -> str:
    # FAIL: $ <input M &>output
    # STDIOS = (sys.stdin, sys.stdout, sys.stderr)
    # return next(os.ttyname(f.fileno()) for f in STDIOS if f.isatty())
    # return "/dev/pts/6"
    return "/dev/tty"


def override_by_tty(f: TextIO) -> TextIO:
    # WKRND: keep tty open until app exit (for simplicity)
    # pylint:disable=consider-using-with
    ftty = open(get_ttyname(), f.mode, encoding=f.encoding)
    os.dup2(ftty.fileno(), f.fileno())
    return ftty


# ALT: Redirect stdout to a file in Python? - Stack Overflow ⌇⡡⣚⣾⠡
#   https://stackoverflow.com/questions/4675728/redirect-stdout-to-a-file-in-python/22434262#22434262
# [_] CMP: /@/research/airy/pacman/pikaur/pikaur/pikspect.py:87: class TTYInputWrapper():
#   SEE: prompt-toolkit and fzf -- how to make a "partial" screen with ncurses ifc
# stdout - How to I make python curses application pipeline friendly? - Stack Overflow ⌇⡡⢬⡺⠄
#   https://stackoverflow.com/questions/53696818/how-to-i-make-python-curses-application-pipeline-friendly
# percol/percol at master · mooz/percol ⌇⡡⢬⡹⣵
#   https://github.com/mooz/percol/tree/master/percol
def rebind_stdios_to_tty() -> dict[str, TextIO]:
    tty = {}
    odummy = io.StringIO()
    for f in (sys.stdin, sys.stdout):  # FAIL:(sys.stderr): not collected
        # HACK: bind initial "fd0" to "tty" for curses and assign new "fd" to old stdin
        nm = f.name[1:-1]
        if f.isatty():
            # NOTE: redirect stdin/stdout even if it's already "tty"
            # HACK: ignore STDIN but collect merged(STDOUT/STDERR) to print on __exit__
            ftty = override_by_tty(f)
            newf = open(os.devnull, f.mode) if f.mode == "r" else odummy
        else:
            oldfd = os.dup(f.fileno())
            ftty = override_by_tty(f)
            try:
                newf = os.fdopen(oldfd, f.mode)
            except OSError:  # wrong mode or /dev/null
                # oldfd = None
                raise
        # HACK: override high-level objects by dups and abandon low-level fileno
        setattr(sys, nm, newf)
        setattr(sys, "__" + nm + "__", newf)
        tty[nm] = ftty
    return tty


class TUI:
    stdscr: C.window
    tty: dict[str, TextIO]
    otty: TextIO

    def __init__(self) -> None:
        if not C.has_extended_color_support():
            raise NotImplementedError

    def __enter__(self) -> "TUI":
        # FIXME:TRY: reassign stdout only temporarily until curses binds itself to TTY
        #   << FAIL? libncurses.so directly uses fd0 inside getch()
        self.tty = rebind_stdios_to_tty()
        self.otty = self.tty["stdout"]

        C.setupterm(term=os.environ.get("TERM", "unknown"), fd=self.otty.fileno())

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

        if isinstance(sys.stdout, io.StringIO):
            otxt = sys.stdout.getvalue()
            sys.stdout.close()
            self.tty["stdout"].write(otxt)
        # if isinstance(sys.stderr, io.StringIO):
        #     etxt = sys.stderr.getvalue()
        #     sys.stderr.close()
        #     self.tty["stderr"].write(etxt)

        for f in self.tty.values():
            if f is not None:
                f.close()
