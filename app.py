import asyncio
import logging
from typing import Any

from .dom import CursorViewWidget
from .tui import TUI

## ARCH:
# * all async only in App
# * reuse as sep sync/async lib
# + inputs:keys/events/cmdpipe/signals/errors
# + outputs:print/draw-canvas/dump/logging
# + mainloop:async


class Application:
    STDIN_FILENO = 0

    def __init__(self, fcoro) -> None:
        self.tasks: list[asyncio.Task] = []
        # self.aws: list[Awaitable] = []
        self.fcoro = fcoro

        self._init()

    def _init(self) -> None:
        self.wg = CursorViewWidget()
        self.ctx = TUI()
        # scr.nodelay(True)  # non-blocking .getch()
        # cb = lambda scr=scr, wg=wg: process_input(scr, wg)
        # asyncio.get_running_loop().add_reader(fd=self.STDIN_FILENO, callback=cb)

    def __enter__(self) -> "Application":
        self.tui = self.ctx.__enter__()
        return self

    def __exit__(self, typ: Any = None, val: Any = None, tb: Any = None) -> None:
        # BAD: should exit manually after 'q' stops loop
        self.ctx.__exit__(typ, val, tb)

    def attach(self) -> None:
        asyncio.get_running_loop().set_exception_handler(self.handle_exception)
        # WARN: mainloop() should be running COS .create_task() immediately schedules
        # TEMP:HACK:IMPL: deferred launch of awaitables
        # for aw in self.aws:
        #     self.tasks.append(asyncio.create_task(aw))
        coro = self.fcoro(self.tui, self.wg)
        self.tasks.append(asyncio.create_task(coro))

    def cancel(self) -> None:
        for t in self.tasks:
            t.cancel()

    async def mainloop(self) -> None:
        self.attach()
        # ALT: use .gather to auto-cancel_all
        # MAYBE: use "timeout=10" and cancel tasks in several attempts
        done, pending = await asyncio.wait(self.tasks)
        assert not pending, pending
        assert done == set(self.tasks), (done, self.tasks)

    def shutdown(self) -> None:
        self.cancel()
        self.__exit__()

    def run(self) -> None:
        asyncio.run(self.mainloop())

    def handle_exception(self, _loop, context):  # type:ignore
        msg = context.get("exception", context["message"])
        logging.error(f"Caught exception: {msg}")
        logging.info("Shutting down...")
        asyncio.get_running_loop().call_soon(self.shutdown)

    # def attach_input(self) -> None:
    #     scr = tui.scr
    #     scr.nodelay(True)  # non-blocking .getch()
    #     STDIN_FILENO = 0
    #     cb = lambda scr=scr, wg=wg: process_input(scr, wg)
    #     asyncio.get_running_loop().add_reader(fd=STDIN_FILENO, callback=cb)

    # def detach_input() -> None:
    #     cancel_all()
    #     asyncio.all_tasks()
    #     # BAD: should exit manually after 'q' stops loop
    #     ctx.__exit__(None, None, None)

    # def attach_output(self) -> None:
    #     ctx = TUI()
    #     tui = ctx.__enter__()
    #     scr = tui.scr
    #     fut = asyncio.create_task(run(tui, wg))
    #     # fut.cancel()
    #     # await run(tui, wg)

    # async def process_output(self) -> None:
    #     while True:
    #         # PERF: don't bind "draw" and "handle" in single loop pass
    #         # IDEA: use .invalidate() to mark region for redraw
    #         #   and semaphor to wait in loop until it's triggered
    #         #   HACK: to prevent too frequent polling/redraw -- measure "dtrun" and "dtwait"
    #         #   and sleep till the end of Vsync frame before applying accumulated changes
    #         draw_all(scr, wg)
    #         await asyncio.sleep(0.1)  # TEMP:REM:
