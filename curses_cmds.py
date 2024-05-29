from typing import Any, Callable

import _curses as C

from . import curses_ext as CE
from .util.logger import log

# def raise_(exc: BaseException) -> None:
#     raise exc


def exitloop(_: Any) -> None:
    # BET? properly exit mainloop
    #   NEED: send kind of "Event/Queue" to rotate loop once
    raise SystemExit()  # CHECK: same as sys.exit()


def resize(myns: dict[str, Any]) -> None:
    ## DEP:configure(--enable-sigwinch)
    # BAD: does not work inside jupyter
    # BAD: should press some key before ev:410 will be reported
    #   ::: it's due to epoll() doesn't listen for SIGWINCH
    #     SEE: /usr/lib/python3.12/site-packages/pexpect/pty_spawn.py
    #   2024-05-11 BUT it works if get_wch() used directly
    # REGR: redraw() during KEY_RESIZE results in ncurses crash
    #   THINK: how to prevent/block redraw in that case?
    myns["stdscr"].refresh()


_primary = None


def asyncio_primary_out(myns: dict[str, Any], coro: Any) -> None:
    global _primary  # pylint:disable=global-statement

    if _primary:
        raise RuntimeError("BUG: primary TTY app is already set")

    stdscr: C.window = myns["stdscr"]

    import asyncio
    import signal

    loop = asyncio.get_running_loop()
    loop.remove_reader(fd=CE.CURSES_STDIN_FD)
    loop.remove_signal_handler(signal.SIGWINCH)

    def _cb(fut: asyncio.Future[int]) -> None:
        try:
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
        finally:
            loop = asyncio.get_running_loop()
            loop.add_signal_handler(signal.SIGWINCH, stdscr.refresh)
            loop.add_reader(CE.CURSES_STDIN_FD, handle_input, myns)
            _primary = None

    # MAYBE: do it immediately before launching SHELL itself
    #   (otherwise #miur may sleep in-between and still leak some input to SHELL)
    C.flushinp()
    _primary = asyncio.create_task(coro)
    _primary.add_done_callback(_cb)


def shell_out(myns: dict[str, Any]) -> None:
    asyncio_primary_out(myns, CE.shell_async(myns["stdscr"]))


def ipykernel_start(myns: dict[str, Any]) -> None:
    from .util.jupyter import inject_ipykernel_into_asyncio

    loop = __import__("asyncio").get_running_loop()
    inject_ipykernel_into_asyncio(loop, myns)


def ipyconsole_out(myns: dict[str, Any]) -> None:
    async def _ipy_async() -> None:
        from .util.jupyter import ipyconsole_async

        with CE.curses_altscreen(myns["stdscr"]):
            await ipyconsole_async()
    asyncio_primary_out(myns, _ipy_async())


def ipython_out(myns: dict[str, Any]) -> None:
    CE.ipython_out(myns["stdscr"])


# ALT: match to string, and then resolve to appropriate function
g_input_handlers: dict[str | int, Callable[[dict[str, Any]], None]] = {
    "\033": exitloop,
    "q": exitloop,
    C.KEY_RESIZE: resize,
    "S": shell_out,  # CE.shell_out
    "K": ipykernel_start,
    "I": ipyconsole_out,
    "\t": ipython_out,
}


def handle_input(myns: dict[str, Any]) -> None:
    wch = myns["stdscr"].get_wch()
    cmd = g_input_handlers.get(wch, None)
    comment = f" ({cmd.__name__})" if cmd else ""
    log.warning(repr(wch) + comment)
    if cmd:
        # WARN: last stmt in loop COS: may raise SystemExit
        cmd(myns)
