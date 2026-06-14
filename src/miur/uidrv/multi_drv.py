import os
import sys
import tempfile
import time
from contextlib import ExitStack, contextmanager
from subprocess import Popen, TimeoutExpired
from types import TracebackType
from typing import Generator, Self, TextIO

from ..systems.tuisystem import DisplayList
from .curses_drv import CursesUIDriver
from .printtext_drv import PrintTextUIDriver


# REF:PRIA: /d/miur/legacy/bc5_miur_asyncio/devhelp/newterm.py
#   BET? /d/miur/&g/newterm.nou
#     SEE: dedicated .py script to send IO/events back'n'forth
@contextmanager
def newtermwindow() -> Generator[tuple[TextIO, TextIO]]:
    with ExitStack() as stack:
        tmpf = stack.enter_context(tempfile.NamedTemporaryFile(mode="r"))
        cmd = 'tty > "$0"; trap "kill -WINCH $1" WINCH; inotifywait -qq -e delete_self "$0"'
        bgtty = stack.enter_context(
            Popen(["st", "-M", "-e", "sh", "-c", cmd, tmpf.name, str(os.getpid())])
        )

        def _cleanup_terminal() -> None:
            try:
                bgtty.wait(timeout=0.2)
            except TimeoutExpired:
                ## WARN: force terminal death before ExitStack tries to wait on it!
                bgtty.terminate()

        stack.callback(_cleanup_terminal)
        ## HACK: close file to exit remote terminal *before* terminating
        stack.callback(tmpf.close)

        time.sleep(0.3)
        tmpf.seek(0)
        ttynm = tmpf.read().strip()

        # pylint:disable=consider-using-with
        rtty = open(ttynm, "r", encoding=sys.stdin.encoding)
        stack.callback(rtty.close)
        wtty = open(ttynm, "w", encoding=sys.stdout.encoding)
        stack.callback(wtty.close)
        yield (rtty, wtty)


# MAYBE: spawn each client in its own thread from the get-go ?
class MultiUIDriver:
    """Multiple side-windows spawner for synchronous navigation on side-monitor"""

    _stack: ExitStack
    cursesdrv: CursesUIDriver
    printdrv: PrintTextUIDriver

    def __enter__(self) -> Self:
        with ExitStack() as stack:
            do = stack.enter_context
            self.cursesdrv = do(CursesUIDriver())
            rtty, wtty = do(newtermwindow())
            self.printdrv = do(PrintTextUIDriver(rtty, wtty))
            self._stack = stack.pop_all()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> bool | None:
        return self._stack.__exit__(exc_type, exc, tb)

    def input(self) -> int | str:
        ## WARN: unless I run printdrv.input() -- keys are echoed
        # return self.printdrv.input()
        return self.cursesdrv.input()

    def sizewh(self) -> tuple[int, int]:
        # return self.printdrv.sizewh()
        return self.cursesdrv.sizewh()

    def clear(self) -> None:
        self.cursesdrv.clear()
        self.printdrv.clear()

    def refresh(self) -> None:
        self.cursesdrv.refresh()
        self.printdrv.refresh()

    def draw_displ(self, displ: DisplayList) -> None:
        # MAYBE: split into different threads to draw in parallel?
        self.cursesdrv.draw_displ(displ)
        self.printdrv.draw_displ(displ)
