import os
import sys
from contextlib import contextmanager
from io import TextIOWrapper

# WARN:PERF: somehow doing import here is 2ms faster, than moving into func-local stmt
from subprocess import CompletedProcess, run
from threading import BoundedSemaphore
from typing import IO, Any, Callable, Final, Iterator, Sequence

import _curses as C

# NOTE: hardcoded inside underlying ncurses C-lib
CURSES_STDIN_FD: Final = 0
CURSES_STDOUT_FD: Final = 1
CURSES_STDERR_FD: Final = 2


class redir_stdio_nm:
    _fdini: int
    _ttyio: IO[Any]
    _conf: dict[str, Any]

    # CHG:(open): allow passing from outside:
    #   * path to be opened with same flags (like currently TTY)
    #   * already opened fd-r/w
    #   * already opened single fd-rw
    def __init__(self, nm: str) -> None:  # ALT: Literal["stdin", "stdout", "stderr"]
        assert nm in ("stdin", "stdout", "stderr"), nm
        self._stdnm = nm
        self._conf = {}

    def __enter__(self) -> None:
        nm = self._stdnm  # OR: nm -> stdio.name[1:-1]
        stdio = getattr(sys, nm)
        assert stdio is getattr(sys, "__" + nm + "__"), "IPython limitation"
        self._fdini = stdio.fileno()
        assert self._fdini == globals()[f"CURSES_{nm.upper()}_FD"], "Sanity check"
        for attr in ("line_buffering", "write_through", "encoding"):
            self._conf[attr] = getattr(stdio, attr)
        fddup = os.dup(self._fdini)
        os.set_inheritable(fddup, False)
        dupio = os.fdopen(fddup, stdio.mode, encoding=stdio.encoding)
        assert isinstance(dupio, TextIOWrapper)
        # MAYBE:(TextIOWrapper.write_through=True): to avoid buffering before piping data
        #   PERF:OPT: allow to change buffering (depending on later measured performance)
        dupio.reconfigure(line_buffering=True, write_through=True)
        setattr(sys, nm, dupio)
        setattr(sys, "__" + nm + "__", dupio)
        # libc = ctypes.CDLL(None)
        # c_stdio = ctypes.c_void_p.in_dll(libc, nm)
        # libc.fflush(c_stdio)
        # WARN: we can't keep it as "old_stdio" to directly restore on __exit__()
        #   COS: os.dup2() will close current underlying fd, making stdio disfunctional
        stdio.close()

        # ATT: we can't reuse "stdio.mode" -- it will be wrong in case of pipe/redir
        #   ! we can't reuse even ".encoding" if redirection was binary inof text
        mode = "r" if nm == "stdin" else "w"
        # OR:DFL: encoding=locale.getencoding() | "utf-8"
        # pylint:disable=consider-using-with
        self._ttyio = open("/dev/tty", mode, encoding="locale")
        # RQ:(inheritable=True): we need FD bound to TTY for shell_out() to work
        os.dup2(self._ttyio.fileno(), self._fdini, inheritable=True)

    def __exit__(self, et=None, exc=None, tb=None):  # type:ignore
        nm = self._stdnm
        dupio = getattr(sys, nm)
        assert dupio is getattr(sys, "__" + nm + "__"), "IPython limitation"
        fddup = dupio.fileno()
        assert fddup != globals()[f"CURSES_{nm.upper()}_FD"], "Sanity check"
        fdini = self._fdini
        os.dup2(fddup, fdini, inheritable=True)
        self._ttyio.close()
        # FIXED(closefd=False):ERR:(on exit):
        #   sys:1: ResourceWarning: unclosed file <_io.TextIOWrapper name=0 mode='r' encoding='utf-8'>
        cf = self._conf
        stdio = os.fdopen(fdini, dupio.mode, closefd=False)
        assert isinstance(stdio, TextIOWrapper)
        stdio.reconfigure(
            encoding=cf["encoding"],
            line_buffering=cf["line_buffering"],
            write_through=cf["write_through"],
        )
        setattr(sys, nm, stdio)
        setattr(sys, "__" + nm + "__", stdio)
        dupio.close()


## ALT: C.wrapper(drawloop: Callable[[C.window], None])
@contextmanager
def curses_stdscr() -> Iterator[C.window]:
    if not C.has_extended_color_support():
        raise NotImplementedError
    try:
        C.setupterm(term=os.environ.get("TERM", "unknown"), fd=CURSES_STDOUT_FD)
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
# @contextmanager
# def curses_altscreen(stdscr: C.window) -> Iterator[None]:
#     """NICE: redirect all logs to primary altscreen"""
#     C.def_prog_mode()  # save current tty modes
#     C.endwin()  # restore original tty modes
#     try:
#         yield
#     finally:
#         stdscr.refresh()  # restore save modes, repaint screen
class curses_altscreen:
    # FIXME! we should control not altscreen, but exclusive access to TTY
    _sema1 = BoundedSemaphore(value=1)

    def __init__(
        self, stdscr: C.window, *, fflush: Callable[[], None] | None = None
    ) -> None:
        self._stdscr = stdscr
        self._flush = fflush

    def __enter__(self) -> None:
        # HACK: throw BUG if you try to altscreen when you are already altscreen (e.g. shell_out)
        #  ALT: inof failing (on bug) OR blocking -- simply print to screen as-is
        if not self._sema1.acquire(blocking=False):
            # sys.exit("BUG")
            ## FAIL: ignored by asyncio
            raise RuntimeError("BUG: altscreen is already switched out")
        C.def_prog_mode()  # save current tty modes
        C.endwin()  # restore original tty modes

    # def __exit__(self,
    #   exc_type: Optional[Type[BaseException]],
    #   exc_value: Optional[BaseException],
    #   traceback: Optional[TracebackType]
    #   ) -> Optional[bool]:
    def __exit__(self, et=None, exc=None, tb=None):  # type:ignore
        # ATT: force immediate output before you switch back to curses alt-screen
        if self._flush:
            self._flush()
        # ALT:TRY: C.doupdate()
        self._stdscr.refresh()  # restore save modes, repaint screen
        self._sema1.release()


@contextmanager
def stdio_to_altscreen(stdscr: C.window) -> Iterator[None]:
    from .util.logger import log

    # WARN:PERF: switching back-n-forth this way takes 0.5ms on each logline
    def _write(oldw: Callable[[str], int], s: str) -> int:
        with curses_altscreen(stdscr, fflush=oldw.__self__.flush):
            return oldw(s)

    oldwout = sys.stdout.write
    oldwerr = sys.stderr.write
    oldlog = log.write
    try:
        # FIXME: replace only if STD* is not redirected already to file or pipe
        #   OR:OPT: especially requesting logs OR output to TTY
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
        ## PERF=57MB
        # with open(f"/proc/{os.getpid()}/smaps", "r") as f:
        #     pssmem = sum(int(l.split()[1]) for l in f.readlines() if l.startswith("Pss:"))
        #     print(pssmem)
        # NOTE: we shouldn't crash on ZSH (or whatever) returning "exitcode=1"
        return run(cmdv, env=envp, check=False, text=True)


async def shell_async(stdscr: C.window, cmdv: Sequence[str] = (), **envkw: str) -> int:
    if not cmdv:
        cmdv = (os.environ.get("SHELL", "sh"),)
    envp = dict(os.environ, **envkw)
    # WARN: #miur can run in bkgr, but is not allowed to interact with TTY
    #   OR:MAYBE: we can allow it -- like create notifications,
    #    or embed small curses popups directly around cursor
    with curses_altscreen(stdscr):
        import asyncio

        # SRC: https://docs.python.org/3/library/asyncio-subprocess.html#examples
        proc = await asyncio.create_subprocess_exec(*cmdv, env=envp)
        rc = await proc.wait()
        assert rc == 0, proc
        return rc


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
