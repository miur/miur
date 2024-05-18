import curses as C
import os
import sys
from contextlib import contextmanager
from subprocess import CompletedProcess, run
from typing import Any, Callable, Iterator, Sequence


## ALT: C.wrapper(drawloop: Callable[[C.window], None])
@contextmanager
def curses_stdscr() -> Iterator[C.window]:
    if not C.has_extended_color_support():
        raise NotImplementedError
    try:
        stdscr = C.initscr()
        C.noecho()  # echoing of keys = off
        C.cbreak()  # buffering on keyboard input = off
        stdscr.keypad(True)  # sup special escape seq for e.g. curses.KEY_LEFT
        C.start_color()  # WAIT: which exception does it throw? TRY: TERM=dummy
        yield stdscr
    finally:
        try:
            stdscr.keypad(False)
            # del stdscr  # TRY? ALT:BAD: not ported :: delscreen(stdscr)
            C.echo()
            # BAD: both nocbreak and endwin may return _curses.error.ERR
            C.nocbreak()
            C.endwin()  # CHECK: is it safe to deinit libncurses multiple times in Jupyter?
        finally:
            pass


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
@contextmanager
def curses_altscreen(stdscr: C.window) -> Iterator[None]:
    """NICE: redirect all logs to primary altscreen"""
    C.def_prog_mode()  # save current tty modes
    C.endwin()  # restore original tty modes
    try:
        yield
    finally:
        # ALT:TRY: C.doupdate()
        stdscr.refresh()  # restore save modes, repaint screen


# def print_curses_altscreen(stdscr: C.window, msg: str) -> None:
#     with curses_altscreen(stdscr):
#         sys.stdout.write(msg)
#         # ATT: force immediate output before you switch back to curses alt-screen
#         sys.stdout.flush()
#
#
# @contextmanager
# def log_to_altscreen(stdscr: C.window) -> Iterator[None]:
#     from .util.logger import log
#
#     # TODO: also STDERR -- for conditional based on level
#     old_stdout = log.write
#     try:
#         log.config(write=lambda text: print_curses_altscreen(stdscr, text))
#         yield
#     finally:
#         log.config(write=old_stdout)


@contextmanager
def stdio_to_altscreen(stdscr: C.window) -> Iterator[None]:
    from .util.logger import log

    def _write(oldw: Callable[[str], int], s: str) -> int:
        C.def_prog_mode()
        C.endwin()
        try:
            return oldw(s)
        finally:
            sys.stdout.flush()
            stdscr.refresh()

    oldwout = sys.stdout.write
    oldwerr = sys.stderr.write
    oldlog = log.write
    try:
        sys.stdout.write = lambda s: _write(oldwout, s)
        sys.stderr.write = lambda s: _write(oldwerr, s)
        # BAD:WKRND: overriding initial class VAR for logger
        log.write = sys.stdout.write
        yield
    finally:
        log.write = oldlog
        sys.stdout.write = oldwout
        sys.stderr.write = oldwerr


def shell_out(
    stdscr: C.window, cmdv: Sequence[str] = (), **envkw: str
) -> CompletedProcess[str]:
    if not cmdv:
        cmdv = (os.environ.get("SHELL", "sh"),)
    envp = dict(os.environ, **envkw)
    with curses_altscreen(stdscr):
        return run(cmdv, env=envp, check=True, text=True)


def ipython_out(stdscr: C.window, user_ns: dict[str, Any] | None = None) -> None:
    if user_ns is None:
        fr = sys._getframe(1)  # pylint:disable=protected-access
        # user_ns = fr.f_globals | fr.f_locals
        user_ns = fr.f_locals

    # pylint:disable=import-outside-toplevel
    import IPython
    from traitlets.config import Config

    c = Config()
    c.InteractiveShell.confirm_exit = False
    c.TerminalIPythonApp.display_banner = False

    with curses_altscreen(stdscr):
        IPython.start_ipython(argv=[], config=c, user_ns=user_ns)
        ## ATT: It's not what I want
        # NameError in list comprehension when using embed · Issue #8918 · ipython/ipython ⌇⡦⠿⢘⢵
        #   https://github.com/ipython/ipython/issues/8918#issuecomment-149898784
        # IPython.embed(config=c, user_ns=user_ns)
