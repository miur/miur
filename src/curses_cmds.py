from typing import Callable
import inspect

import _curses as C

from . import curses_ext as CE
from .app import AppGlobals
from .loop_asyncio import asyncio_primary_out
from .util.logger import log

# def raise_(exc: BaseException) -> None:
#     raise exc


def exitloop(g: AppGlobals) -> None:
    # raise SystemExit()  # OR:(same): import sys; sys.exit()
    g.doexit()


def resize(g: AppGlobals) -> None:
    # HACK: force size reinit (as ncurses only does it in .iniscr())
    # SRC:OLD: https://post.bytes.com/forum/topic/python/541442-curses-and-resizing-windows
    C.def_prog_mode()
    C.endwin()
    g.stdscr.refresh()
    # HACK: remove KEY_RESIZE from queue to avoid multi-refresh after several resizes
    #   ALT:FAIL: _ch = g.stdscr.getch(); assert _ch == C.KEY_RESIZE, _ch
    C.flushinp()

    log.info("resize: [{}x{}]".format(*g.stdscr.getmaxyx()))
    ## DEP:configure(--enable-sigwinch)
    # BAD: does not work inside jupyter
    # BAD: should press some key before ev:410 will be reported
    #   ::: it's due to epoll() doesn't listen for SIGWINCH
    #     SEE: /usr/lib/python3.12/site-packages/pexpect/pty_spawn.py
    #   2024-05-11 BUT it works if get_wch() used directly
    # REGR: redraw() during KEY_RESIZE results in ncurses crash
    #   THINK: how to prevent/block redraw in that case?
    g.stdscr.clear()  # CHECK:NEED:OR:NOT? e.g. to clear bkgr (which earlier wasn't redrawn on resize)
    g.root_wdg.resize(g.stdscr)
    g.stdscr.refresh()


def shell_out(g: AppGlobals) -> None:
    asyncio_primary_out(g, CE.shell_async(g.stdscr))


def ipykernel_start(g: AppGlobals) -> None:
    from .util.jupyter import inject_ipykernel_into_asyncio

    loop = __import__("asyncio").get_running_loop()
    # pylint:disable=protected-access
    myns = {"g": g, "stdscr": g.stdscr, "_main": g._main}
    inject_ipykernel_into_asyncio(loop, myns)


def ipyconsole_out(g: AppGlobals) -> None:
    async def _ipy_async() -> None:
        from .util.jupyter import ipyconsole_async

        with CE.curses_altscreen(g.stdscr):
            await ipyconsole_async()

    asyncio_primary_out(g, _ipy_async())


def ipython_out(g: AppGlobals) -> None:
    CE.ipython_out(g.stdscr)


# ALT: match to string, and then resolve to appropriate function
g_input_handlers: dict[str | int, Callable[[AppGlobals], None]] = {
    "\033": exitloop,
    "q": exitloop,
    C.KEY_RESIZE: resize,
    "S": shell_out,  # CE.shell_out
    "K": ipykernel_start,
    "I": ipyconsole_out,
    "j": lambda g: g.root_wdg.cursor_move_rel(1),
    "k": lambda g: g.root_wdg.cursor_move_rel(-1),
    "\t": ipython_out,
}


def handle_input(g: AppGlobals) -> None:
    wch = g.stdscr.get_wch()
    # IDEA: partially restore TTY to preserve NLs in unexpected exc/backtrace
    #  C.nocbreak() ... C.cbreak()
    cmd = g_input_handlers.get(wch, None)
    if cmd:
        if (nm := cmd.__name__) == '<lambda>':
            comment = " : " + inspect.getsource(cmd).partition('lambda')[2].partition(':')[2].strip(' ,\n')
        else:
            comment = f" ({nm})"
    else:
        comment = ""
    log.warning(repr(wch) + comment)
    # print(repr(wch))
    # import sys; sys.stdout.write(repr(wch))
    if cmd:
        # WARN: last stmt in loop COS: may raise SystemExit
        cmd(g)
        # CHG: only do partial redraw e.g. prev/next cursor areas
        # MAYBE: redraw only if anything had changed (not all cmds to that)
        #   BUT: uix needs visual feedback on each keypress, so it's better to always redraw
        g.root_wdg.redraw(g.stdscr)
        g.stdscr.refresh()
