import os
import sys
from contextlib import contextmanager

# WARN:PERF: somehow doing import here is 2ms faster, than moving into func-local stmt
from subprocess import CompletedProcess, run
from threading import BoundedSemaphore
from typing import Any, Callable, Iterator, Sequence

import _curses as C


# TEMP?HACK: dump logs to altscreen
def dump_logbuf_to_tty() -> None:
    from .app import g_app

    assert g_app.io.ttyout, "TEMP: Should always exist"

    if (alt := g_app.io.ttyalt) and (tout := g_app.io.ttyout):
        # BET? shutil.copyfileobj(alt, tout)
        #   SRC: https://stackoverflow.com/questions/3253258/what-is-the-best-way-to-write-the-contents-of-a-stringio-to-a-file
        if buf := alt.getvalue():
            tout.write(buf)
            tout.flush()
            # PERF:BET? creating a new one instead of reusing a blank one is 11% faster
            #   SRC: https://stackoverflow.com/questions/4330812/how-do-i-clear-a-stringio-object
            alt.truncate(0)


## ALT: C.wrapper(drawloop: Callable[[C.window], None])
@contextmanager
def curses_stdscr() -> Iterator[C.window]:
    if not C.has_extended_color_support():
        raise NotImplementedError

    from . import iomgr

    C.setupterm(term=os.environ.get("TERM", "unknown"), fd=iomgr.CURSES_STDOUT_FD)
    try:
        stdscr = C.initscr()
        C.noecho()  # echoing of keys = off
        C.cbreak()  # buffering on keyboard input = off
        stdscr.keypad(True)  # sup special escape seq for e.g. curses.KEY_LEFT
        C.start_color()  # WAIT: which exception does it throw? TRY: TERM=dummy
        stdscr.nodelay(True)
        yield stdscr
    finally:
        try:
            stdscr.nodelay(False)
            stdscr.keypad(False)
            # del stdscr  # TRY? ALT:BAD: not ported :: delscreen(stdscr)
            C.echo()
            # BAD: both nocbreak and endwin may return _curses.error.ERR
            C.nocbreak()
            C.endwin()  # CHECK: is it safe to deinit libncurses multiple times in Jupyter?
        finally:
            # TEMP:HACK: dump logs on app exit
            #   BAD? probably doesn't belong here, but whatever
            dump_logbuf_to_tty()


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
        dump_logbuf_to_tty()

    # def __exit__(self,
    #   exc_type: Optional[Type[BaseException]],
    #   exc_value: Optional[BaseException],
    #   traceback: Optional[TracebackType]
    #   ) -> Optional[bool]:
    def __exit__(self, et=None, exc=None, tb=None):  # type:ignore[no-untyped-def]
        # ATT: force immediate output before you switch back to curses alt-screen
        if self._flush:
            self._flush()
        # ALT:TRY: C.doupdate()
        self._stdscr.refresh()  # restore save modes, repaint screen
        self._sema1.release()


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
        # [_] TODO: ..., stdin=g_app.io.ttyin, stdout=g_app.io.ttyout, stderr=(g_app.io.pipeerr or g_app.io.ttyalt))
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
        # [_] TODO: ..., stdin=g_app.io.ttyin, stdout=g_app.io.ttyout, stderr=(g_app.io.pipeerr or g_app.io.ttyalt))
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
