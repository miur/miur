import os
import sys
import tempfile
import time
from contextlib import ExitStack, contextmanager
from queue import Queue
from subprocess import Popen, TimeoutExpired
from threading import Thread
from types import TracebackType
from typing import Generator, Self, TextIO

from ..uicommon.displaylist import DisplayList
from .curses_drv import CursesUIDriver
from .printtext_drv import PrintTextUIDriver


# REF:PRIA: /d/miur/legacy/bc5_miur_asyncio/devhelp/newterm.py
#   BET? /d/miur/&g/newterm.nou
#     SEE: dedicated .py script to send IO/events back'n'forth
@contextmanager
def new_termwindow() -> Generator[tuple[TextIO, TextIO]]:
    with ExitStack() as stack:
        tmpf = stack.enter_context(tempfile.NamedTemporaryFile(mode="r"))
        cmd = 'tty > "$0"; trap "kill -WINCH $1" WINCH; inotifywait -qq -e delete_self "$0"'
        bgtty = stack.enter_context(
            # DISABLED:("-M"): I need to scroll history up
            Popen(["st", "-e", "sh", "-c", cmd, tmpf.name, str(os.getpid())])
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


@contextmanager
def new_guithread() -> Generator[tuple[Queue[str], Queue[str]]]:
    # RENAME? sendqt,recvqt
    to_qt: Queue[str] = Queue()
    from_qt: Queue[str] = Queue()

    def _qt_tgt() -> None:
        # RQ: all Qt stuff MUST be imported in GUI thread first
        from .qt6wdg_drv import main

        main(to_qt, from_qt)

    qthr = Thread(target=_qt_tgt, daemon=True)  # , args=(to_qt, from_qt)
    qthr.start()
    try:
        yield (to_qt, from_qt)
    finally:
        to_qt.put("SHUTDOWN")
        qthr.join(timeout=3)
        if qthr.is_alive():
            print("WARN: qt thread did not exit cleanly within timeout")


# MAYBE: spawn each client in its own thread from the get-go ?
class MultiUIDriver:
    """Multiple side-windows spawner for synchronous navigation on side-monitor"""

    _stack: ExitStack
    cursesdrv: CursesUIDriver
    printdrv: PrintTextUIDriver
    # qt6wdgdrv: ...
    to_qt: Queue[str]
    from_qt: Queue[str]

    def __enter__(self) -> Self:
        with ExitStack() as stack:
            do = stack.enter_context
            self.cursesdrv = do(CursesUIDriver())
            rtty, wtty = do(new_termwindow())
            self.printdrv = do(PrintTextUIDriver(rtty, wtty))
            (self.to_qt, self.from_qt) = do(new_guithread())
            self.to_qt.put("INIT")
            # reply = self.from_qt.get()
            # print(reply)
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
