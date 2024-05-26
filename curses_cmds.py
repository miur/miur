import curses as C
from typing import Callable

from . import curses_ext as CE
from .util.logger import log

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
    import signal

    curses_stdin_fd = 0
    loop = asyncio.get_running_loop()
    loop.remove_reader(fd=curses_stdin_fd)
    loop.remove_signal_handler(signal.SIGWINCH)
    C.flushinp()

    def _cb(fut: asyncio.Future[int]) -> None:
        globals()["_primary"] = None
        sfx = " (shell_out)"
        if fut.cancelled():
            log.warning("cancelled" + sfx)
        elif exc := fut.exception():
            exc.add_note(sfx)
            from .util.exchook import exception_handler

            exception_handler(type(exc), exc, exc.__traceback__)
            C.flash()
        else:
            log.info(f"rc={fut.result()}{sfx}")

        C.flushinp()
        loop = asyncio.get_running_loop()
        loop.add_signal_handler(signal.SIGWINCH, scr.refresh)
        loop.add_reader(fd=curses_stdin_fd, callback=lambda: handle_input(scr))

    _primary = asyncio.create_task(CE.shell_async(scr))
    _primary.add_done_callback(_cb)


def ipython_out(scr: C.window) -> None:
    CE.ipython_out(scr)


# ALT: match to string, and then resolve to appropriate function
g_input_handlers: dict[str | int, Callable[[C.window], None]] = {
    "\033": exitloop,
    "q": exitloop,
    C.KEY_RESIZE: resize,
    "S": shell_out,
    "\t": ipython_out,
}


def handle_input(stdscr: C.window) -> None:
    wch = stdscr.get_wch()
    cmd = g_input_handlers.get(wch, None)
    comment = f" ({cmd.__name__})" if cmd else ""
    log.warning(repr(wch) + comment)
    if cmd:
        # WARN: last stmt in loop COS: may raise SystemExit
        cmd(stdscr)
