from collections.abc import Iterable
from types import TracebackType
from typing import TYPE_CHECKING, Self, TextIO

from ..uicommon.displaylist import DisplayStream


# RENAME? {Connection,Message}UIDriver
#   WHY: primitive clients like CLI/REPL, !netcat, etc. may negotiate to use
#     curses_drv or printtext_drv, instead of interpreting msgs on their own
#   ALSO: we may need different _drv for Qt and OpenGL with different elements
# SPLIT: socket vs ZMQ
class ClientUIDriver:
    def __init__(self, rfd: TextIO | None = None, wfd: TextIO | None = None) -> None:
        self._rfd = rfd
        self._wfd = wfd

    def __enter__(self) -> Self:
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> bool | None:
        pass

    def input(self) -> str:
        return ""

    def sizewh(self) -> tuple[int, int]:
        return 1, 1

    def clear(self) -> None:
        self.draw_lines(["\n"])

    def refresh(self) -> None:
        pass

    def draw_lines(self, lines: Iterable[str]) -> None:
        self._wfd.writelines(lines)
        self._wfd.flush()

    def draw_status(self, text: str) -> None:
        self._wfd.writelines(text)
        self._wfd.flush()

    def draw_displ(self, displ: DisplayStream) -> None:
        self.draw_lines(str(displ))


if TYPE_CHECKING:
    from .base_drv import BaseUIDriver

    _instance: BaseUIDriver = ClientUIDriver()
    _factory: type[BaseUIDriver] = ClientUIDriver
