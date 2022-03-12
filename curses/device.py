import curses as C
from contextlib import ExitStack, contextmanager
from typing import Any, Iterator

from ..devhelp.fdredir import bind_fd01_from_tty
from ..devhelp.newterm import newtermwindow


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
        # BAD: not ported :: delscreen(stdscr)
        del stdscr  # TRY
        C.echo()
        C.nocbreak()
        # CHECK: is it safe to deinit libncurses multiple times in Jupyter?
        C.endwin()


class CursesDevice:
    def __init__(self) -> None:
        self.rwtty: Any
        self.scr: C.window
        self._stack: ExitStack

    def __enter__(self) -> "CursesDevice":
        with ExitStack() as stack:
            self.rwtty = stack.enter_context(newtermwindow())
            stack.enter_context(bind_fd01_from_tty(*self.rwtty))
            self.scr = stack.enter_context(makestdscr())
            self._stack = stack.pop_all()
        self._init(self.scr)
        return self

    # def __exit__(self,
    #   exc_type: Optional[Type[BaseException]],
    #   exc_value: Optional[BaseException],
    #   traceback: Optional[TracebackType]
    #   ) -> Optional[bool]:
    def __exit__(self, t=None, v=None, b=None):  # type:ignore
        return self._stack.__exit__(t, v, b)

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
