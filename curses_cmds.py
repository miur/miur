import curses as C
from typing import Callable

from . import curses_ext as CE

# def raise_(exc: BaseException) -> None:
#     raise exc


def exitloop(_: C.window) -> None:
    # BET? properly exit mainloop
    #   NEED: send kind of "Event/Queue" to rotate loop once
    raise SystemExit()  # CHECK: same as sys.exit()


def resize(scr: C.window) -> None:
    ## DEP:configure(--enable-sigwinch)
    # BAD: does not work inside jupyter
    # BAD: should press some key before ev:410 will be reported
    #   ::: it's due to epoll() doesn't listen for SIGWINCH
    #     SEE: /usr/lib/python3.12/site-packages/pexpect/pty_spawn.py
    #   2024-05-11 BUT it works if get_wch() used directly
    # REGR: redraw() during KEY_RESIZE results in ncurses crash
    #   THINK: how to prevent/block redraw in that case?
    scr.refresh()


_primary = None

def shell_out(scr: C.window) -> None:
    # CE.shell_out(scr)
    import asyncio
    _primary = asyncio.create_task(CE.shell_async(scr))
    _primary.add_done_callback(lambda *a: None)


def ipython_out(scr: C.window) -> None:
    CE.ipython_out(scr)


# ALT: match to string, and then resolve to appropriate function
input_handlers: dict[str | int, Callable[[C.window], None]] = {
    "\033": exitloop,
    "q": exitloop,
    C.KEY_RESIZE: resize,
    "S": shell_out,
    "\t": ipython_out,
}
