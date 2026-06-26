from collections.abc import Callable
from contextlib import ExitStack
from queue import Queue
from types import TracebackType
from typing import Self

from ..uicommon.displaylist import DisplayList
from .connectors.termpipe import new_termwindow
from .connectors.threadqueue import new_guithread
from .curses_drv import CursesUIDriver
from .printtext_drv import PrintTextUIDriver


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
            self.cursesdrv = do(CursesUIDriver())
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
        return self.cursesdrv.input()
        # return self.printdrv.input()

    def sizewh(self) -> tuple[int, int]:
        return self.cursesdrv.sizewh()
        # return self.printdrv.sizewh()

    def clear(self) -> None:
        self.cursesdrv.clear()
        self.printdrv.clear()

    def refresh(self) -> None:
        self.cursesdrv.refresh()
        self.printdrv.refresh()
        self.send_to_qt("REDRAW")

    def draw_displ(self, displ: DisplayList) -> None:
        # MAYBE: split into different threads to draw in parallel?
        self.cursesdrv.draw_displ(displ)
        self.printdrv.draw_displ(displ)
