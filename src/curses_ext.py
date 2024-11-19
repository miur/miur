import os
import sys
from contextlib import contextmanager

# WARN:PERF: somehow doing import here is 2ms faster, than moving into func-local stmt
from subprocess import CompletedProcess, run
from threading import BoundedSemaphore
from types import SimpleNamespace
from typing import Any, Callable, Iterator, Sequence

import _curses as C

from . import iomgr
from .app import g_app

# SEE: https://bugs.python.org/issue40284
# CHECK: it seems "S.<Tab>" completion doesn't work
g_style = SimpleNamespace()
_registered_color_pairs: dict[tuple[int, int], int] = {}


def termcolor2(fg: int, bg: int) -> int:
    fgbg = (fg, bg)
    p = _registered_color_pairs.get(fgbg, None)
    if p is None:
        i = len(_registered_color_pairs)
        # CHECK: if newer curses supports more than 256 color pairs
        # REF: https://stackoverflow.com/questions/476878/256-colors-foreground-and-background
        assert i < 256
        C.init_pair(i, fg, bg)
        p = _registered_color_pairs[fgbg] = C.color_pair(i)
    return p


def init_colorscheme(stdscr: C.window) -> None:
    # print(C.COLORS)
    # if C.COLORS < 8:
    #     C.init_pair(1, 7, 0)
    #     C.init_pair(2, 4, 6)
    # else:
    C.use_default_colors()

    S = g_style
    S.hardcoded = termcolor2(C.COLOR_WHITE, C.COLOR_BLACK)  # C.color_pair(0)

    S.default = termcolor2(-1, -1)  # DFL: gray text on transparent bkgr
    S.item = S.default
    S.auxinfo = termcolor2(10, -1)
    S.iteminfo = termcolor2(0, -1)
    S.pfxrel = S.auxinfo
    S.pfxidx = S.iteminfo
    S.cursor = C.A_REVERSE | C.A_BOLD  # OR: termcolor2(8, 4)
    S.footer = termcolor2(217, 17)
    S.error = termcolor2(160, -1)  # 1
    S.fsdir = termcolor2(33, -1)  # 4
    S.fslink = termcolor2(37, -1)  # 6
    S.fsexe = termcolor2(64, -1)  # 2

    # pvis = C.curs_set(visibility=0)
    stdscr.attron(S.default)


## ALT: C.wrapper(drawloop: Callable[[C.window], None])
@contextmanager
def curses_stdscr() -> Iterator[C.window]:
    if not C.has_extended_color_support():
        raise NotImplementedError

    # import traceback as TR
    # from .util.logger import log

    # NOTE: only activate stderr=ttyalt when ncurses is active, otherwise print immediately
    # sys.stderr = g_app.io.ttyalt

    C.setupterm(term=os.environ.get("TERM", "unknown"), fd=iomgr.CURSES_STDOUT_FD)
    try:
        stdscr = C.initscr()
        C.noecho()  # echoing of keys = off
        C.cbreak()  # buffering on keyboard input = off
        stdscr.keypad(True)  # sup special escape seq for e.g. curses.KEY_LEFT
        C.start_color()  # WAIT: which exception does it throw? TRY: TERM=dummy
        stdscr.nodelay(True)
        init_colorscheme(stdscr)

        ## HACK: allow ANSI colors in curses.addstr()
        # SRC: https://github.com/getcuia/cusser
        # FAIL: doesn't work on !python>=3.11
        # from cusser import Cusser
        # if not isinstance(stdscr, Cusser):
        #     stdscr = Cusser(stdscr)

        yield stdscr
    # except Exception as exc:
    #     log.error("E1: " + "".join(TR.format_exception(exc, chain=True)))
    #     raise
    finally:
        try:
            stdscr.refresh()
            stdscr.nodelay(False)
            stdscr.keypad(False)
            # del stdscr  # TRY? ALT:BAD: not ported :: delscreen(stdscr)
            C.echo()
            # BAD: both nocbreak and endwin may return _curses.error/ERR
            C.nocbreak()
            C.endwin()  # CHECK: is it safe to deinit libncurses multiple times in Jupyter?
        # except Exception as exc:
        #     log.error("E2: " + "".join(TR.format_exception(exc, chain=True)))
        #     raise
        finally:
            # TEMP:HACK: dump logs on app exit
            #   BAD? probably doesn't belong here, but whatever
            # iomgr.dump_logbuf_to_tty()
            # sys.stderr = g_app.io.ttyout
            pass


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

        iomgr.dump_logbuf_to_tty(g_app)

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
    interactive = False
    if not cmdv:
        cmdv = (os.environ.get("SHELL", "sh"),)
        interactive = True
    envp = dict(os.environ, **envkw)
    # WARN: #miur can run in bkgr, but is not allowed to interact with TTY
    #   OR:MAYBE: we can allow it -- like create notifications,
    #    or embed small curses popups directly around cursor
    with curses_altscreen(stdscr):
        import asyncio

        from .app import g_app as g

        # SRC: https://docs.python.org/3/library/asyncio-subprocess.html#examples
        proc = await asyncio.create_subprocess_exec(
            *cmdv,
            env=envp,
            stdin=g.io.ttyin,
            stdout=g.io.ttyout,
            stderr=(g.io.pipeerr or g.io.ttyout),
        )
        rc = await proc.wait()
        if rc:
            from .util.logger import log

            msg = f"{rc=} <- {cmdv} {proc}"
            log.error(msg)
            if not interactive:
                raise RuntimeError(msg)
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
