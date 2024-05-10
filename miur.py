import sys
import os
from subprocess import run
import curses as C
from contextlib import contextmanager  # ExitStack,
from typing import Iterator, Iterable

# from just.ext.logging import logcfg, L

from .log import log

# @contextmanager
# def makestdscr() -> Iterator[C.window]:
#     try:
#         stdscr = C.initscr()
#         yield stdscr
#     finally:
#         C.endwin()


@contextmanager
def curses_altscreen(stdscr: C.window) -> Iterator[None]:
    ## NICE: redirect all logs to primary altscreen
    C.def_prog_mode()  # save current tty modes
    C.endwin()  # restore original tty modes

    ## FAIL: clears bkgr, moves cursor, +/- doesn't show old buffer
    # clear = "\033[H"
    # mainscr = "\033[?1049l"
    # altscr = "\033[?1049h"
    # print(mainscr + "mylogline" + altscr, end="", file=__import__("sys").stderr)
    try:
        yield
    finally:
        stdscr.refresh()  # restore save modes, repaint screen


def shell_out(stdscr: C.window, cmdv: Iterable[str] = (), **envkw: str) -> None:
    cmd = cmdv or [os.environ.get("SHELL", "sh")]
    envp = dict(os.environ, **envkw)
    with curses_altscreen(stdscr):
        _rc = run(cmd, env=envp, check=True)


def print_curses_altscreen(stdscr: C.window, msg: str):
    with curses_altscreen(stdscr):
        sys.stdout.write(msg)
        # ATT: force immediate output before you switch back to curses alt-screen
        sys.stdout.flush()


def drawloop(stdscr: C.window) -> None:
    log.config(write=lambda text: print_curses_altscreen(stdscr, text))

    if not C.has_extended_color_support():
        raise NotImplementedError
    try:
        # stdscr.nodelay(True)
        # self._loop.add_reader(fd=self.STDIN_FILENO, callback=self.process_input)
        while True:
            try:
                wch = stdscr.get_wch()
            except C.error:
                break
            except KeyboardInterrupt:
                break
            if wch == "q":
                break
            if wch == "s":
                shell_out(stdscr)
            log.warning(lambda: f"{wch}")

    finally:
        stdscr.nodelay(False)


# class Application:
#     @C.wrapper
#     def __enter__(self) -> Self:
#         return self
#
#     def __exit__(self, *exc_details) -> bool:
#         return self._stack.__exit__(*exc_details)


# TBD: frontend to various ways to run miur API with different UI
def miur(cwd: str) -> None:
    log.info(f"{cwd=}")
    # with Application() as app:
    return C.wrapper(drawloop)


def _live() -> None:
    pass
