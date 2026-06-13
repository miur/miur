import shutil
import sys
from typing import Self, assert_never

from wcwidth import clip, width

from ..systems.tuisystem import Aid, DisplayStream, TextSpan
from .ansicolor import ansicolor

# ALT: https://pypi.org/project/readkeys/
if sys.platform == "win32":
    import msvcrt  # pylint:disable=import-error

    def _get_wch() -> str:
        ch = msvcrt.getwch()  # Reads a single Unicode character
        # Special keys (arrows, F-keys) emit a null or 0xe0 byte first
        if ch in ("\000", "\xe0"):
            msvcrt.getwch()  # Consume the extended key code
            return "TBD"  # Or handle arrow/function keys here
        return ch
else:  # Unix implementation (Linux/macOS)
    import termios
    import tty

    def _get_wch() -> str:
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(fd)
            ch = sys.stdin.read(1)  # Reads one character
            # Check for multi-byte sequences (e.g., arrow keys often start with \x1b)
            if ch == "\x1b":
                ch += sys.stdin.read(2)  # Read the next 2 bytes of the sequence
        finally:
            # MAYBE: instead of single char -- switch stdin mode permanently in __enter/exit ?
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch


class TermStyle:
    reset = ansicolor()
    item = default = ansicolor(-1, -1)
    footer = ansicolor(217, 17, bold=True)
    # itempunct = lambda s:


# SEP/OPT::
#   * text (vs serialized DisplayList/AST dump for fmt-converters)
#   * unicode vs ascii
#   * plain (vs my own enriched markup) (vs standard markup format sections)
#   * aligned monospace cell-grid (vs unaligned sans-font (vs cell2pixel-positioning))
#   * boundary none/insert/crop
#   * append-only stdout (vs file-seek in-place edits)
#   * print whole-frame (vs append-changes)
#   * nocolor vs termcolor/style[8/16/256/64K/RGB] (vs rawterm-passthrough/strip-termcodes) (vs VT100-subset)
#   * last-frame only (vs continuous worklog [append chunks] -- to email/troubleshoot interactions later)
class PrintTextUIDriver:
    termstyle: TermStyle

    def __init__(self) -> None:
        self.print = print

    def __enter__(self) -> Self:
        self.termstyle = TermStyle()
        return self

    def __exit__(self, *_a: object) -> None:
        pass

    def input(self) -> str:
        return _get_wch()

    def sizewh(self) -> tuple[int, int]:
        return shutil.get_terminal_size(fallback=(80, 24))

    # MAYBE? strip term-codes in draw_lines() and make draw_rawterm() to allow them
    def draw_lines(self, lines: list[str]) -> None:
        self.print("".join(lines))

    def draw_displ(self, displ: DisplayStream) -> None:
        self.draw_lines(self.rasterize_displ(self.pad_boundary(displ, 80, 120)))

    def rasterize_displ(self, displ: DisplayStream) -> list[str]:
        py = 0
        nx = 0
        s = ""
        lines: list[str] = []
        for token in displ:
            match token:
                case TextSpan(x, y, text, wc, aid):
                    # DEBUG: print(token, nx)
                    if (ljump := y - py) < 0:
                        raise NotImplementedError("moving up == potential overlap")
                    if ljump > 0:
                        lines.append(s + "\n" * ljump)  # OR:(decor): f"\n[..{ljump}]"
                        s = ""
                        nx = 0
                    if (cjump := x - nx) < 0:
                        raise NotImplementedError("moving left == potential overlap")
                    if cjump > 0:
                        s += " " * cjump
                    if aid != Aid.default:
                        # FIXME: use "parametrized style" here ?
                        #   OR: define generic rainbow in TermStyle and map range of Aid indexes to it
                        text = f"\033[3{aid}m" + text + "\033[m"

                    attr = getattr(self.termstyle, Aid(aid).name)
                    s += attr + text + self.termstyle.reset
                    py = y
                    nx = x + wc
                case _:
                    # TODO: log errors and continue; MAYBE store errs in list and return them
                    # TODO: visually render some read bars in their assumed locations,
                    #   derived on ordered list of prev/next tokens
                    assert_never(token)
        if s:
            lines.append(s)
        return lines

    def pad_boundary(self, displ: DisplayStream, wfit: int, wmax: int) -> DisplayStream:
        boundary = "|"  # ALT="|\n↪"
        bounw = width(boundary)
        py = 0
        nx = 0
        # WARN: list needs to be sorted by .x
        # ALT:PERF: for large lists use bisect()
        for tok in displ:
            assert isinstance(tok, TextSpan)
            x, y, wc = tok.x, tok.y, tok.wc
            # WARN!BAD? prev token isn't guaranteed to be rightmost
            if y > py:
                if (spacer := wfit - nx) > 0:
                    # ALT:(string): l = wcwidth.ljust(l, wfit, " ") + "|"
                    yield TextSpan(nx, py, " " * spacer, spacer, Aid.default)
                if spacer >= 0:
                    yield TextSpan(nx + spacer, py, boundary, bounw, Aid.default)
                nx = 0
                py = y

            nx = x + wc
            if nx < min(wfit, wmax):
                yield tok
            elif nx >= wfit:
                yield tok
            else:
                yield tok

        if (spacer := wfit - nx) >= 0:
            yield TextSpan(nx, py, " " * spacer, spacer, Aid.default)

    def _pad_line_boundary(  # pylint:disable=too-many-arguments,too-many-positional-arguments
        self, displ: DisplayStream, cx: int, cy: int, wfit: int, wmax: int
    ) -> int:
        broken_token_idx = -1
        for i in range(len(displ) - 1, -1, -1):
            t = displ[i]
            if t.y != cy:
                break  # <CASE: single line only
            # FAIL:FIXME: splice if exactly bw two tokens
            if t.x <= wfit < t.x + t.wc:
                broken_token_idx = i
                break  # <FIXME? also split multiple overlapping tokens
        if broken_token_idx != -1:
            t = displ[broken_token_idx]
            # FAIL:(only works on ascii): l = l[:wfit] + "|" + l[wfit:]
            a = clip(t.t, 0, wfit - t.x, fillchar="·")
            b = clip(t.t, wfit - t.x, wmax - wfit, fillchar="·")
            wa = width(a)
            displ[broken_token_idx : broken_token_idx + 1] = [
                t._replace(t=a, wc=wa),
                TextSpan(t.x + wa, t.y, boundary, bounw, Aid.default),
                t._replace(t=b, wc=width(b)),
            ]
