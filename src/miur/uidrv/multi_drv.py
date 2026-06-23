import os
import sys
import tempfile
import time
from collections.abc import Callable, Generator
from contextlib import ExitStack, contextmanager
from queue import Empty, Queue
from subprocess import Popen, TimeoutExpired
from types import TracebackType
from typing import Self, TextIO

from .. import log
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
            log.info("un-term")
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
def new_guithread() -> Generator[
    tuple[Callable[[object], None], Queue[dict[str, object]]]
]:
    """
    Context manager that spins up a PySide6 GUI thread and yields a thread-safe
    send function and a receive queue.
    """
    from threading import Event, Thread

    # OLD:RENAME? (to_qt,from_qt) | (sendqt,recvqt)
    send_bridge: list[Callable[[object], None]] = []
    recv_q: Queue[dict[str, object]] = Queue()

    # Synchronization event to prevent yielding before Qt is ready
    ready_event = Event()

    def _qt_tgt() -> None:
        # RQ: all Qt stuff MUST be imported in GUI thread first
        from .qt6wdg_drv import main

        main(send_bridge, recv_q, ready_event)

    qthr = Thread(target=_qt_tgt, daemon=True)  # , args=(to_qt, from_qt)
    qthr.start()

    # Corner Case: Block main thread until the GUI thread completes QApplication setup
    ready_event.wait(timeout=3)
    if not send_bridge:
        # TEMP:HACK: crash on access to empty list if Qt had failed
        raise RuntimeError("Qt seems to crash?")

    def send_fn(item: object) -> None:
        # Corner Case: Prevent calls if the emitter has been cleared during shutdown
        if send_bridge:
            send_bridge[0](item)

    try:
        # Yield the bound signal.emit directly. Main thread sees Callable[[Any], None].
        yield (send_fn, recv_q)
    finally:
        exit_cookie = "SHUTDOWN"
        send_fn(exit_cookie)
        try:
            while recv_q.get(timeout=3)["ev"] != exit_cookie:
                pass
            log.info(exit_cookie)
        except Empty:
            log.warning("WARN: can't safely close Qt")

        # WARN:ARCH: actually, this thread should NEVER end after first ever Qt import
        #   >> if you wish to recreate Qt window AGAIN -- you need *same* thread
        #   IDEA: only join on !miur exit, and all other time -- keep thread running in a loop,
        #     and waiting for new msgs from send_fn() to re-spawn Qt app again
        qthr.join(timeout=3)
        log.info("join")
        if qthr.is_alive():
            print("WARN: qt thread is running aft timeout", file=sys.stderr)


# MAYBE: spawn each client in its own thread from the get-go ?
class MultiUIDriver:
    """Multiple side-windows spawner for synchronous navigation on side-monitor"""

    _stack: ExitStack
    cursesdrv: CursesUIDriver
    printdrv: PrintTextUIDriver
    # qt6wdgdrv: ...
    send_to_qt: Callable[[object], None]
    qt_recv_q: Queue[dict[str, object]]

    def __enter__(self) -> Self:
        with ExitStack() as stack:
            do = stack.enter_context
            # self.cursesdrv = do(CursesUIDriver())
            rtty, wtty = do(new_termwindow())
            self.printdrv = do(PrintTextUIDriver(rtty, wtty))
            (self.send_to_qt, self.qt_recv_q) = do(new_guithread())
            self.send_to_qt("INIT")
            # reply = self.qt_recv_q.get()
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
        return self.printdrv.input()
        # return self.cursesdrv.input()

    def sizewh(self) -> tuple[int, int]:
        return self.printdrv.sizewh()
        # return self.cursesdrv.sizewh()

    def clear(self) -> None:
        # self.cursesdrv.clear()
        self.printdrv.clear()

    def refresh(self) -> None:
        # self.cursesdrv.refresh()
        self.printdrv.refresh()

    def draw_displ(self, displ: DisplayList) -> None:
        # MAYBE: split into different threads to draw in parallel?
        # self.cursesdrv.draw_displ(displ)
        self.printdrv.draw_displ(displ)
