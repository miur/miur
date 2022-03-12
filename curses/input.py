import asyncio
import curses as C
from typing import Any

from ..fragments import handle_keybindings
from .device import CursesDevice
from .output import CursesOutput


class CursesInput:
    STDIN_FILENO = 0

    def __init__(
        self, app: "Application", iodev: CursesDevice, canvas: CursesOutput
    ) -> None:
        self.app = app
        self.scr = iodev.scr
        self.wg = app.wg
        self.canvas = canvas

    @property
    def _loop(self) -> asyncio.AbstractEventLoop:
        return asyncio.get_running_loop()

    def __enter__(self) -> "CursesInput":
        # non-blocking .getch()
        self.scr.nodelay(True)
        self._loop.add_reader(fd=self.STDIN_FILENO, callback=self.process_input)
        return self

    def __exit__(self, *_exc: Any) -> None:
        self._loop.remove_reader(fd=self.STDIN_FILENO)
        self.scr.nodelay(False)

    def process_input(self) -> None:
        # NOTE: only count events, don't accumulate into list for post-processing
        #   << otherwise new events received during processing will become stalled
        #   FUTURE:TRY: read events in bursts (nested while loop) to be able to cancel-out
        #     BAD:PERF: we may constantly linger in this loop, never yielding to asyncio
        #     MAYBE: current single loop already allows "bursts" and giving up CPU time ?
        evnum = 0
        # HACK:(getch-ungetch-getkey): avoid throwing exception
        while (ch := self.scr.getch()) != C.ERR:
            evnum += 1
            # if ch == C.KEY_RESIZE:
            #     print(ch)
            C.ungetch(ch)
            key = self.scr.get_wch()
            # key = scr.getkey()
            print(repr(key), file=__import__("sys").stderr)

            ## DEP:configure(--enable-sigwinch)
            # BAD: does not work inside jupyter
            # BAD: should press some key before ev:410 will be reported
            # REGR: redraw() during KEY_RESIZE results in ncurses crash
            #   THINK: how to prevent/block redraw in that case?
            if key == C.KEY_RESIZE:
                self.canvas.resize()
                continue

            if key in ("q", "\033"):  # "d",
                # [_] SEIZE: A Python asyncio cancellation pattern | by Rob Blackbourn | Medium ⌇⡢⠨⢣⣉
                #   https://rob-blackbourn.medium.com/a-python-asyncio-cancellation-pattern-a808db861b84
                # OLD: fstop = cast(asyncio.Task, asyncio.current_task()).cancel
                # ALT:NICE: works even in Jupyter
                self.app.cancel()
            handle_keybindings(self.wg, key)
            self.canvas.invalidate()

        if evnum == 0:
            print("WTF: processing woke up w/o input", file=__import__("sys").stderr)
