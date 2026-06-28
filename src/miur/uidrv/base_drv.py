from types import TracebackType
from typing import Protocol, Self

from ..uicommon.displaylist import DisplayStream


class BaseUIDriver(Protocol):
    def __enter__(self) -> Self: ...

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> bool | None: ...

    def input(self) -> str: ...

    def sizewh(self) -> tuple[int, int]: ...

    def clear(self) -> None: ...

    def refresh(self) -> None: ...

    def draw_lines(self, lines: list[str]) -> None: ...

    def draw_displ(self, displ: DisplayStream) -> None: ...
