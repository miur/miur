import asyncio
import signal
from contextlib import contextmanager
from typing import Any, Callable, Iterator, cast

from . import iomgr
from .app import AppGlobals
from .util.exchook import exception_handler, log_exc
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
        # FIXME: don't cancel "shutdown" task
        #   BET: gather results of the rest of cancelled tasks, and then set shutdown event
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
    # myloop.slow_callback_duration = 0.1
    ## BUG: overwrites Jupyter defaults and extends Application GC lifetime
    ## FAIL: is not triggered if taskref was stored to variable [outside the loop]
    myloop.set_exception_handler(_custom_exception_handler)
    asyncio.set_event_loop(myloop)
    try:
        yield myloop
    finally:
        log.trace("setting... loop=None")
        ## ALT:BAD~
        # for t in asyncio.all_tasks(myloop):
        #     t.cancel()
        # log.info(str(asyncio.all_tasks()))
        asyncio.set_event_loop(None)
        # with asyncio.Runner(loop_factory=lambda: myloop):
        #     pass


async def mainloop_asyncio(g: AppGlobals) -> None:
    g.curses_ui.resize()

    loop = asyncio.get_running_loop()
    # FAIL: RuntimeError: Event loop stopped before Future completed.
    ev_shutdown = asyncio.Event()

    def _doexit() -> None:
        g.exiting = True
        ev_shutdown.set()

    g.doexit = _doexit

    loop.add_signal_handler(signal.SIGINT, ev_shutdown.set)
    # FIND:MAYBE: don't process in handler directly, and only schedule callback ?
    loop.add_signal_handler(signal.SIGWINCH, g.curses_ui.resize)
    loop.add_reader(iomgr.CURSES_STDIN_FD, g.curses_ui.handle_input)
    try:
        log.kpi("serving asyncio")
        if __debug__ and g.opts.PROFILE_STARTUP:
            loop.call_soon(ev_shutdown.set)
        await ev_shutdown.wait()
        # while True:
        #     await asyncio.sleep(1)
    except asyncio.CancelledError:
        ## TEMP: suppress unnecessary CancelledError Traceback after "_custom_exception_handler"
        ##   BAD: mistakenly swallows exception even if smb else (mistakenly?) did t.cancel()
        # print(exc, file=g.io.ttyout)
        pass
    finally:
        log.trace("removing... fds/signals")
        loop.remove_reader(fd=iomgr.CURSES_STDIN_FD)
        loop.remove_signal_handler(signal.SIGWINCH)
        loop.remove_signal_handler(signal.SIGINT)


# BET: assign app.curses_ui -- when it's created
#   i.e. treat "None" as our own app doesn't ~need~ TTY
#     = so it's free to be taked by anybody at any moment
_primary = None


def asyncio_primary_out(
    g: AppGlobals,
    coro: Any,
    cb: Callable[[], None] | None = None,
) -> None:
    global _primary  # pylint:disable=global-statement

    # MAYBE: combine with "curses_altscreen/BoundedSemaphore" -- as they *are* related
    #   ~~ though app may not need full-screen curses access, it still uses same TTY
    if _primary:
        del coro  # FIXED:(suppress):ERR: sys:1: RuntimeWarning: coroutine 'shell_async' was never awaited
        raise RuntimeError("BUG: primary TTY app is already set")

    import _curses as C

    loop = asyncio.get_running_loop()
    loop.remove_reader(fd=iomgr.CURSES_STDIN_FD)
    loop.remove_signal_handler(signal.SIGWINCH)

    def _cb(fut: asyncio.Future[int]) -> None:
        success = False
        try:
            sfx = " (shell_out)"
            if fut.cancelled():
                log.warning("cancelled" + sfx)
            elif exc := fut.exception():
                exc.add_note(sfx)
                log_exc(exc)
                C.flash()
            else:
                log.info(f"rc={fut.result()}{sfx}")
                success = True

            C.flushinp()
        finally:
            loop = asyncio.get_running_loop()
            loop.add_signal_handler(signal.SIGWINCH, g.curses_ui.resize)
            loop.add_reader(iomgr.CURSES_STDIN_FD, g.curses_ui.handle_input)
            global _primary  # pylint:disable=global-statement
            _primary = None
        # HACK: chain callback() hook/stmt after successfully executed task
        if success and cb:
            try:
                cb()
            except Exception as exc:  # pylint:disable=broad-exception-caught
                log_exc(exc)

    # MAYBE: do it immediately before launching SHELL itself
    #   (otherwise #miur may sleep in-between and still leak some input to SHELL)
    C.flushinp()
    _primary = asyncio.create_task(coro)
    _primary.add_done_callback(_cb)
