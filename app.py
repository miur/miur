import asyncio
import logging
from contextlib import ExitStack
from typing import Any

from .curses.device import CursesDevice
from .curses.input import CursesInput
from .curses.output import CursesOutput
from .dom import CursorViewWidget

## ARCH:
# * all async only in App
# * reuse as sep sync/async lib
# + inputs:keys/events/cmdpipe/signals/errors
# + outputs:print/draw-canvas/dump/logging
# + mainloop:async


class Application:
    STDIN_FILENO = 0

    iodev: CursesDevice
    canvas: CursesOutput
    hotkey: CursesInput
    _stack: ExitStack

    def __init__(self) -> None:
        # RENAME: dom
        #   BUT: dom(wg) is common only between all list-like displays
        #     -- concept of cursor won't have any sense in API, CLI, or AUDIO
        self.wg = CursorViewWidget()
        self._tasks: list[asyncio.Task] = []
        # self.aws: list[Awaitable] = []

    # ---
    def __enter__(self) -> "Application":
        with ExitStack() as stack:
            self.iodev = stack.enter_context(CursesDevice())  # OR: self.ctx()
            # FAIL: lifetime should not continue past __exit__()
            self.canvas = CursesOutput(self.iodev, self.wg)
            self.hotkey = CursesInput(self, self.iodev, self.canvas)
            self._stack = stack.pop_all()
        return self

    def __exit__(self, t=None, v=None, b=None) -> bool:  # type:ignore
        # FAIL: lifetime should not continue past __exit__()
        del self.hotkey
        del self.canvas
        return self._stack.__exit__(t, v, b)

    # ---
    def attach(self) -> None:
        ## BUG: overwrites Jupyter defaults and extends Application GC lifetime
        # asyncio.get_running_loop().set_exception_handler(self.handle_exception)

        # WARN: mainloop() should be running COS .create_task() immediately schedules
        # TEMP:HACK:IMPL: deferred launch of awaitables
        # for aw in self.aws:
        #     self._tasks.append(asyncio.create_task(aw))
        coro = self.canvas.drawloop()
        self._tasks.append(asyncio.create_task(coro))
        self.hotkey.__enter__()

    # FUTURE:MAYBE: wait for tasks being cancelled
    def cancel(self) -> None:
        for t in self._tasks:
            t.cancel()
        # NOTE: even if tasks won't be cancelled -- you can access them by .all_tasks()
        #   NEED: for Jupyter re-launch ++ no-op .cancel
        self._tasks = []

    # ---
    def startup(self) -> None:
        self.__enter__()
        self.attach()
        # HACK: autoclose newterm() to restore WM visual space
        #   OR: never close newterm() -- to keep WM layout stable
        self._tasks[0].add_done_callback(lambda *_: self.__exit__())
        self.canvas.resize()

    def shutdown(self) -> None:
        self.cancel()
        self.hotkey.__exit__()
        self.__exit__()

    # ---
    async def wait(self) -> None:
        # ALT: use .gather to auto-cancel_all
        # MAYBE: use "timeout=10" and cancel tasks in several attempts
        aws = self._tasks
        done, pending = await asyncio.wait(aws)
        assert not pending, pending
        assert done == set(aws), (done, aws)

    async def mainloop(self, ainit: Any = None) -> None:
        if ainit:
            ainit()
        self.attach()
        try:
            await self.wait()
        finally:
            self.hotkey.__exit__()
            # if ainit:
            #     ainit(False)

    def run(self, ainit: Any = None) -> None:
        asyncio.run(self.mainloop(ainit))

    # ---
    # def handle_exception(self, _loop, context):  # type:ignore
    #     msg = context.get("exception", context["message"])
    #     logging.error(f"Caught exception: {msg}")
    #     logging.info("Shutting down...")
    #     asyncio.get_running_loop().call_soon(self.shutdown)
