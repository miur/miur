import shutil
from typing import Self


class PrintTextUIDriver:
    def __init__(self) -> None:
        self.print = print

    def __enter__(self) -> Self:
        return self

    def __exit__(self, *_a: object) -> None:
        pass

    def sizewh(self) -> tuple[int, int]:
        return shutil.get_terminal_size(fallback=(80, 24))

    def draw(self, lines: list[str]) -> None:  # CHG? bytes
        self.print("".join(lines))
