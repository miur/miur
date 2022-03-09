import asyncio

## ARCH:
# * all async only in App
# * reuse as sep sync/async lib
# + inputs:keys/events/cmdpipe/signals/errors
# + outputs:print/draw-canvas/dump/logging
# + mainloop:async


class Application:
    def __init__(self) -> None:
        self.tasks: list[asyncio.Task] = []
        self.init()

    def run(self) -> None:
        asyncio.run(self.mainloop())

    async def mainloop(self) -> None:
        # done, pending = await asyncio.wait(aws, timeout=10)
        aws = self.tasks
        done, pending = await asyncio.wait(aws)
        assert not pending
        assert done == aws

    def init(self) -> None:
        wg = CursorViewWidget()
        with TUI() as tui:
            pass

    def attach_input(self) -> None:
        scr = tui.scr
        scr.nodelay(True)  # non-blocking .getch()
        STDIN_FILENO = 0
        cb = lambda scr=scr, wg=wg: process_input(scr, wg)
        asyncio.get_running_loop().add_reader(fd=STDIN_FILENO, callback=cb)

    def detach_input() -> None:
        cancel_all()
        asyncio.all_tasks()
        # BAD: should exit manually after 'q' stops loop
        ctx.__exit__(None, None, None)

    def attach_output(self) -> None:
        ctx = TUI()
        tui = ctx.__enter__()
        scr = tui.scr
        fut = asyncio.create_task(run(tui, wg))
        # fut.cancel()
        # await run(tui, wg)

    async def process_output(self) -> None:
        while True:
            # PERF: don't bind "draw" and "handle" in single loop pass
            # IDEA: use .invalidate() to mark region for redraw
            #   and semaphor to wait in loop until it's triggered
            #   HACK: to prevent too frequent polling/redraw -- measure "dtrun" and "dtwait"
            #   and sleep till the end of Vsync frame before applying accumulated changes
            draw_all(scr, wg)
            await asyncio.sleep(0.1)  # TEMP:REM:
