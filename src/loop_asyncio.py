import asyncio
import signal
from contextlib import contextmanager
from typing import Any, Iterator, cast

from . import iomgr
from .app import AppGlobals
from .curses_cmds import handle_input
from .util.exchook import exception_handler
from .util.logger import log


@contextmanager
def my_asyncio_loop(debug: bool = True) -> Iterator[asyncio.AbstractEventLoop]:

    def _custom_exception_handler(
        loop: asyncio.AbstractEventLoop,
        context: dict[str, Any],
    ) -> None:
        loop.default_exception_handler(context)
        exc = cast(Exception, context.get("exception"))
        exception_handler(type(exc), exc, exc.__traceback__)
        # print(context)
        for t in asyncio.all_tasks():
            t.cancel()
        # loop.stop()

    ## HACK:TRY: combine my default selectors loop with asyncio
    ##   FAIL: asyncio will govern .select() inof @me
    # class MyPolicy(asyncio.DefaultEventLoopPolicy):
    #     def new_event_loop(self):
    #         selector = selectors.DefaultSelector()
    #         return asyncio.SelectorEventLoop(selector)
    # asyncio.set_event_loop_policy(MyPolicy())

    # FIXED:ERR: DeprecationWarning: There is no current event loop
    #   /usr/lib/python3.12/site-packages/tornado/ioloop.py:274:
    myloop = asyncio.new_event_loop()

    myloop.set_debug(debug)
    ## BUG: overwrites Jupyter defaults and extends Application GC lifetime
    ## FAIL: is not triggered if taskref was stored to variable [outside the loop]
    myloop.set_exception_handler(_custom_exception_handler)
    asyncio.set_event_loop(myloop)
    try:
        yield myloop
    finally:
        asyncio.set_event_loop(None)


async def mainloop_asyncio(g: AppGlobals) -> None:
    g.stdscr.refresh()  # CHG:> app.refresh

    loop = asyncio.get_running_loop()
    # FAIL: RuntimeError: Event loop stopped before Future completed.
    ev_shutdown = asyncio.Event()
    loop.add_signal_handler(signal.SIGINT, ev_shutdown.set)
    # FIND:MAYBE: don't process in handler directly, and only schedule callback ?
    loop.add_signal_handler(signal.SIGWINCH, g.stdscr.refresh)  # CHG:> app.refresh
    loop.add_reader(iomgr.CURSES_STDIN_FD, handle_input, g)
    try:
        log.kpi("serving")
        if __debug__ and g.opts.PROFILE_STARTUP:
            loop.call_soon(ev_shutdown.set)
        await ev_shutdown.wait()
        # while True:
        #     await asyncio.sleep(1)
    finally:
        loop.remove_reader(fd=iomgr.CURSES_STDIN_FD)
        loop.remove_signal_handler(signal.SIGWINCH)
        loop.remove_signal_handler(signal.SIGINT)


_primary = None


def asyncio_primary_out(g: AppGlobals, coro: Any) -> None:
    global _primary  # pylint:disable=global-statement

    if _primary:
        raise RuntimeError("BUG: primary TTY app is already set")

    import _curses as C

    loop = asyncio.get_running_loop()
    loop.remove_reader(fd=iomgr.CURSES_STDIN_FD)
    loop.remove_signal_handler(signal.SIGWINCH)

    def _cb(fut: asyncio.Future[int]) -> None:
        try:
            sfx = " (shell_out)"
            if fut.cancelled():
                log.warning("cancelled" + sfx)
            elif exc := fut.exception():
                exc.add_note(sfx)
                exception_handler(type(exc), exc, exc.__traceback__)
                C.flash()
            else:
                log.info(f"rc={fut.result()}{sfx}")

            C.flushinp()
        finally:
            loop = asyncio.get_running_loop()
            loop.add_signal_handler(signal.SIGWINCH, g.stdscr.refresh)
            loop.add_reader(iomgr.CURSES_STDIN_FD, handle_input, g)
            _primary = None

    # MAYBE: do it immediately before launching SHELL itself
    #   (otherwise #miur may sleep in-between and still leak some input to SHELL)
    C.flushinp()
    _primary = asyncio.create_task(coro)
    _primary.add_done_callback(_cb)
