import shutil
import sys
from typing import Iterator, Self, assert_never

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
                        # RED!.
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
        # pylint:disable=too-many-locals
        boundary = "|"  # ALT="|\n↪"
        bounw = width(boundary)
        tailw = max(0, wmax - wfit)
        aid = Aid.default

        # py = 0
        y = None
        nx = 0
        marked = False

        def mark() -> Iterator[TextSpan]:
            nonlocal nx, marked
            if y is None or marked:
                return

            if (spacer := wfit - nx) > 0:
                # ALT:(string): l = wcwidth.ljust(l, wfit, " ") + "|"
                yield TextSpan(nx, y, " " * spacer, spacer, aid)
                nx = wfit
            ## ALT: if y > py:
            # if spacer >= 0:
            #     yield TextSpan(nx + spacer, py, boundary, bounw, Aid.default)
            yield TextSpan(nx, y, boundary, bounw, aid)
            nx += bounw
            marked = True
            # nx = 0
            # py = y

        # WARN: list needs to be sorted by .x
        # ALT:PERF: for large lists use bisect()
        for tok in displ:
            assert isinstance(tok, TextSpan)

            x, ty, wc = tok.x, tok.y, tok.wc
            end = x + wc

            # WARN!BAD? prev token isn't guaranteed to be rightmost
            if ty != y:
                if y is not None:
                    yield from mark()
                y, nx, marked = ty, 0, False

            # Fully before boundary, including exact end at boundary.
            if end <= wfit:
                yield tok
                nx = max(nx, end)

                if end == wfit and not marked:
                    yield from mark()

            # Fully after boundary.
            elif x >= wfit:
                yield from mark()
                yield tok._replace(x=x + bounw)
                nx = max(nx, x + bounw + wc)

            # Crosses boundary: x < wfit < end.
            else:
                cut = wfit - x

                # FAIL:(only works on ascii): l = l[:wfit] + "|" + l[wfit:]
                left = clip(tok.t, 0, cut, fillchar="·")
                lw = width(left)
                if left:
                    yield tok._replace(t=left, wc=lw)

                nx = max(nx, x + lw)
                yield from mark()

                if tailw:
                    right = clip(tok.t, cut, tailw, fillchar="·")
                    rw = width(right)
                    if right:
                        yield tok._replace(x=x + lw + bounw, t=right, wc=rw)
                        nx = max(nx, x + lw + bounw + rw)

        if y is not None:
            yield from mark()
