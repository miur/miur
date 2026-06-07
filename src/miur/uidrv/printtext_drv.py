import shutil
from typing import Self, assert_never

from ..systems.tuisystem import DisplayList, TextSpan


# SEP/OPT::
#   * text (vs serialized DisplayList/AST dump for fmt-converters)
#   * plain (vs my own enriched markup) (vs standard markup format sections)
#   * aligned monospace cell-grid (vs unaligned sans-font (vs cell2pixel-positioning))
#   * append-only stdout (vs file-seek in-place edits)
#   * print whole-frame (vs append-changes)
#   * termcolor/style[8/16/64K/RGB] (vs none/rawterm/stripterm)
#   * last-frame only (vs continuous worklog [append chunks] -- to email/troubleshoot interactions later)
class PrintTextUIDriver:
    def __init__(self) -> None:
        self.print = print

    def __enter__(self) -> Self:
        return self

    def __exit__(self, *_a: object) -> None:
        pass

    def sizewh(self) -> tuple[int, int]:
        return shutil.get_terminal_size(fallback=(80, 24))

    # MAYBE? strip term-codes in draw_lines() and make draw_rawterm() to allow them
    def draw_lines(self, lines: list[str]) -> None:
        self.print("".join(lines))

    def draw_displ(self, displ: DisplayList) -> None:
        py = 0
        nx = 0
        s = ""
        lines: list[str] = []
        for token in displ:
            match token:
                case TextSpan(x, y, text, wc, _sid):
                    if ljump := y - py:
                        assert ljump > 0
                        lines.append(s + "\n" * ljump)
                        s = ""
                        nx = 0
                    if (cjump := x - nx) < 0:
                        raise NotImplementedError
                    s += text + " " * cjump
                    py = y
                    nx = x + wc
                case _:
                    assert_never(token)
        if s:
            lines.append(s)
        self.draw_lines(lines)
