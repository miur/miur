import curses as C
from contextlib import ExitStack, contextmanager
from typing import Any, ContextManager, Iterator

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


@contextmanager
def ScreenNcurses() -> Iterator[C.window]:
    with (
        newtermwindow() as rwtty,
        bind_fd01_from_tty(*rwtty),
        makestdscr() as stdscr,
    ):
        yield stdscr


class TUI:
    def __init__(self) -> None:
        self.gen: ContextManager
        self.scr: C.window
        self._stack: ExitStack

    def __enter__(self) -> "TUI":
        with ExitStack() as stack:
            self.scr = stack.enter_context(ScreenNcurses())
            self._stack = stack.pop_all()
        self._init(self.scr)
        return self

    def __exit__(self, exc_type: Any, exc_value: Any, traceback: Any) -> None:
        self._stack.__exit__(exc_type, exc_value, traceback)

    @staticmethod
    def _init(scr: C.window) -> None:
        # print(C.COLORS)
        # if C.COLORS < 8:
        #     C.init_pair(1, 7, 0)
        #     C.init_pair(2, 4, 6)
        # else:
        C.init_pair(1, 7, 8)
        C.init_pair(2, 8, 4)

        # pvis = C.curs_set(visibility=0)
        scr.attron(C.color_pair(1))
        scr.clear()
        scr.refresh()

        ## [_] TRY: stdout/stderr -> normal curses window, instead of fullscreen alt
        ## [_] SEE: how !ranger does this when jumping into shell
        ### WTF: does Alternate screen works or not ?
        # tput = lambda s: tui.otty.write(C.tigetstr(s).decode(tui.otty.encoding))
        # tput("rmcup")
        # print(C.LINES)
        # tput("smcup")

        # C.napms(1500)
