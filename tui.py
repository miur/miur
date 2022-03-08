import curses as C
from contextlib import contextmanager
from typing import Iterator

from .fdredir import bind_fd01_from_tty
from .newterm import newtermwindow

## HACK
# try:
#     __IPYTHON__
# except NameError:
#     __IPYTHON__ = False
# if __IPYTHON__:
#     pass


@contextmanager
def makestdscr() -> Iterator[C.window]:
    if not C.has_extended_color_support():
        raise NotImplementedError
    try:
        ## DISABLED:(setupterm): using it before initscr is only !python hack for errs
        # OR:(term=os.environ.get("TERM", "unknown"), fd=wtty.fileno())
        # C.setupterm(*args, **kw)
        stdscr = C.initscr()  # ALT: newterm(NULL, outfd, infd)
        C.noecho()  # echoing of keys = off
        C.cbreak()  # buffering on keyboard input = off
        stdscr.keypad(True)  # sup special escape seq for e.g. curses.KEY_LEFT
        try:
            C.start_color()  # ignorable
        except:  # pylint:disable=bare-except
            pass
        yield stdscr
    finally:
        stdscr.keypad(False)
        C.echo()
        C.nocbreak()
        C.endwin()
        # BAD: not ported :: delscreen(stdscr)
        # TRY
        del stdscr


# class TUI:
#     def __init__(self) -> None:
#         pass
#     def __enter__(self) -> "TUI":
#         self.stdscr = self._init()
#         return self
#     def __exit__(self, exc_type: Any, exc_value: Any, traceback: Any) -> None:
#         pass
@contextmanager
def TUI() -> Iterator[C.window]:
    with (
        newtermwindow() as rwtty,
        bind_fd01_from_tty(*rwtty),
        makestdscr() as stdscr,
    ):
        yield stdscr
        ## DEBUG
        # rtty.fileno()
        # wtty.fileno()
        # sys.stdin.fileno()
        # sys.stdout.fileno()
        # sys.__stdin__.fileno()
        # sys.__stdout__.fileno()
