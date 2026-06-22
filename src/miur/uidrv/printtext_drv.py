import os
import sys
from typing import Iterable, Iterator, Self, TextIO, assert_never

from wcwidth import clip, width

from ..uicommon.ansicolor import RESET, ansicolor
from ..uicommon.displaylist import DisplayStream, TextSpan
from ..uicommon.styleids import Aid, StyleId
from ..uicommon.stylesheet import Effect, UnitedStylesheet

# ALT: https://pypi.org/project/readkeys/
if sys.platform == "win32":
    import msvcrt  # pylint:disable=import-error

    def _get_wch(rfd: TextIO, wfd: TextIO) -> str:
        _ = rfd, wfd
        ch = msvcrt.getwch()  # Reads a single Unicode character
        # Special keys (arrows, F-keys) emit a null or 0xe0 byte first
        if ch in ("\000", "\xe0"):
            msvcrt.getwch()  # Consume the extended key code
            return "TBD"  # Or handle arrow/function keys here
        return ch
else:  # Unix implementation (Linux/macOS)
    import termios
    import tty

    def _get_wch(rfd: TextIO, wfd: TextIO) -> str:
        fd = wfd.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(fd)
            ch = rfd.read(1)  # Reads one character
            # Check for multi-byte sequences (e.g., arrow keys often start with \x1b)
            if ch == "\x1b":
                ch += rfd.read(2)  # Read the next 2 bytes of the sequence
        finally:
            # MAYBE: instead of single char -- switch stdin mode permanently in __enter/exit ?
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch


# RENAME? TextStreamUIDriver
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
    style_by_id: list[Effect[str] | None]

    def __init__(self, rfd: TextIO | None = None, wfd: TextIO | None = None) -> None:
        self._rfd = rfd or sys.stdin
        self._wfd = wfd or sys.stdout  # OR: store _writelines to reduce API surface

    def __enter__(self) -> Self:
        self.style_by_id = []
        return self

    def __exit__(self, *_a: object) -> None:
        pass

    def input(self) -> str:
        return _get_wch(self._rfd, self._wfd)

    def sizewh(self) -> tuple[int, int]:
        # REF: shutil.get_terminal_size(fallback=(80, 24))
        w = int(os.environ.get("COLUMNS", "0"))
        h = int(os.environ.get("LINES", "0"))
        if w <= 0 or h <= 0:
            try:
                wo, ho = os.get_terminal_size(self._wfd.fileno())
            except AttributeError, ValueError, OSError:
                wo, ho = (80, 24)
            if w <= 0:
                w = wo
            if h <= 0:
                h = ho
        return w, h

    def clear(self) -> None:
        pass

    def refresh(self) -> None:
        pass

    # MAYBE? strip term-codes in draw_lines() and make draw_rawterm() to allow them
    def draw_lines(self, lines: Iterable[str]) -> None:
        self._wfd.writelines(lines)
        self._wfd.flush()

    def draw_displ(self, displ: DisplayStream) -> None:
        # TODO: use .sizewh (nof 120) to avoid wrapping in terminal, and "max=None" for textstream
        #   MAYBE? use "1024" for text logfiles (to avoid too long lines)
        # displ = self.hi_punct(displ, "[-_.]")
        displ = self.pad_boundary(displ, 70, 120)
        self.draw_lines(self.rasterize_displ(displ))

    def resolve_style(self, aid: StyleId) -> Effect[str]:
        c = UnitedStylesheet.resolve_by_aid(aid)
        if c.effn:
            text2idx = c.effn

            def _effect(text: str) -> str:
                idx = text2idx(text)
                if isinstance(c.fg, (tuple, list)):
                    fg = c.fg[idx % len(c.fg)]
                else:
                    fg = idx  # TEMP:HACK: use idx as termcolor index
                if isinstance(c.bg, (tuple, list)):
                    bg = c.bg[idx % len(c.bg)]
                else:
                    bg = -1
                return ansicolor(
                    fg=fg, bg=bg, bold=c.bold, italic=c.italic, uline=c.uline
                )

            return _effect

        assert isinstance(c.fg, int) and isinstance(c.bg, int)
        # FIXME:DEV: write proper resolver of StyleDef to ANSI
        ansistr = ansicolor(
            fg=c.fg, bg=c.bg, bold=c.bold, italic=c.italic, uline=c.uline
        )
        return ansistr

    def rasterize_displ(self, displ: DisplayStream) -> list[str]:
        if __debug__:
            self.style_by_id = []
        styles = self.style_by_id

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

                    # ALT? cache by StyleDef to auto-invalidate on jurigged update
                    #   BAD? stores refs to IMPL -- which may need to be hotreloaded too
                    # ansistr = styledef2ansi[c]
                    if Aid.sanitize(aid) >= len(styles):
                        styles += [None] * (aid + 1 - len(styles))
                    if (eff := styles[aid]) is None:
                        eff = styles[aid] = self.resolve_style(aid)
                    # FAIL: for multi-colors requires xfm to multiple tokens
                    ansistr = eff(text) if callable(eff) else eff

                    s += ansistr + text + RESET
                    py = y
                    nx = x + wc
                case _:
                    # TODO: log errors and continue; MAYBE store errs in list and return them
                    # TODO: visually render some read bars in their assumed locations,
                    #   derived on ordered list of prev/next tokens
                    assert_never(token)
        if s:
            lines.append(s + "\n")
        return lines

    def pad_boundary(self, displ: DisplayStream, wfit: int, wmax: int) -> DisplayStream:
        # pylint:disable=too-many-locals
        boundary = "|"  # ALT="|\n↪"
        bounw = width(boundary)
        aid = Aid.DEFAULT
        wfit = min(wfit, wmax)

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
            #     yield TextSpan(nx + spacer, py, boundary, bounw, Aid.DEFAULT)
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

            if end <= wfit:  # <CASE: fully before boundary
                yield tok
                nx = max(nx, end)
                if end == wfit and not marked:
                    yield from mark()
            elif x >= wfit:  # <CASE: fully after boundary
                yield from mark()
                yield tok._replace(x=x + bounw)
                nx = max(nx, x + bounw + wc)
            else:  # <CASE: crossed boundary (x < wfit < end)
                cutoff = wfit - x
                # FAIL:(only works on ascii): l = l[:wfit] + "|" + l[wfit:]
                left = clip(tok.t, 0, cutoff, fillchar="·")
                lw = width(left)
                if left:
                    yield tok._replace(t=left, wc=lw)

                nx = max(nx, x + lw)
                yield from mark()
                if (roomw := wmax - nx) > 0:
                    right = clip(tok.t, cutoff, cutoff + roomw, fillchar="·")
                    rw = width(right)
                    if right:
                        yield tok._replace(x=nx, t=right, wc=rw)
                        nx = max(nx, nx + rw)
        if y is not None:
            yield from mark()
