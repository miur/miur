import os
import re
import shutil
from dataclasses import dataclass
from time import monotonic_ns
from typing import NamedTuple, assert_never

## PERF: 45ms(bare) -> 75ms(typing) -> 95ms(shutil,time) -> 250ms(wcwidth)
# ALT:(copy-paste): //site-packages/_pytest/_io/wcwidth.py
from wcwidth import clip, width

from .filecontent import FileContentProxy

type DisplayList = list[TextSpan]


class TextSpan(NamedTuple):  # RENAME: CellSpan
    x: int
    y: int
    t: str
    wc: int  # <MAYBE? cache cell-width hint for renderer's BoundingBox
    sid: int = 0  # MAYBE: split into fg/bg styles? BUT: curses groups them
    zi: int = 1  # < Z-index of whole layer bucket (NOT per-element)
    ## ALT:
    # @property
    # def wc(self) -> int:
    #     return width(self.t)


@dataclass(slots=True)
class VisibleArea:  # pylint:disable=too-many-instance-attributes
    beg_cursor: int
    end_hint: int
    end_actual: int = 0
    wnd_w: int = 0
    wnd_h: int = 0
    vp_w: int = 0
    vp_h: int = 0
    vp_x: int = 0
    vp_y: int = 0


@dataclass(slots=True, kw_only=True)
class Item:
    text: str
    idx: int
    h: object


def make_printable(text: str) -> str:
    # VIZ: https://en.wikipedia.org/wiki/List_of_Unicode_characters#Control_Pictures
    # text = text.rstrip("\r\n") or " "
    text = text.replace(os.linesep, "␤")  # "↵¬"
    # text = text.replace("\t", "▸ ").replace("\x7f", "␡").replace(" ", "␣")
    NUL = ord("␀")
    SP = ord(" ")
    return "".join(chr(NUL + ci) if (ci := ord(c)) < SP else c for c in text)


class MiurKernel:
    # def exec(self, op: object) -> object:
    #     return None
    def lfs_listdir(self, h: str) -> list[str]:
        with os.scandir(h) as it:
            return [x.name for x in it]

    # BAD: where to cache mm/ino ? In OpenedFilesSystem to limit fd-open resources ?
    #   DECI: or combine with "scroll/view state" ?
    def lfs_file_content(self, h: str) -> FileContentProxy:
        return FileContentProxy(h)

    def view_len_items(self, h: str) -> int:
        # FIXME: should be able to return Infinity or Unknown
        return self.lfs_file_content(h).count_lines()

    def view_get_item(self, h: str, i: int) -> Item:
        # if self.order_by:
        #     self._lst.sort(key=self.order_by)  # OR key=str
        # FIXME: cache loff to continue seeking next line
        #   OR:BET? new func to iterate from starting line
        proxy = self.lfs_file_content(h)
        loff = proxy.seek_fwd_to_line_nth(i)
        line = next(proxy.read_lines(1, offset=loff))
        return Item(text=line, idx=i, h=(loff, line))

    def tui_render_term_strings(self, displ: DisplayList) -> list[str]:
        py = 0
        nx = 0
        l = ""
        strings: list[str] = []
        for token in displ:
            match token:
                case TextSpan(x, y, text, wc, sid):
                    # DEBUG: mydrv_print(x, y, text, wc, sid)
                    # print(token, nx)
                    if nl := y - py:
                        assert nl > 0
                        strings.append(l + "\n" * nl)
                        l = ""
                        nx = 0
                    assert x == nx
                    if sid == 0:
                        l += text
                    else:
                        l += f"\033[3{sid}m" + text + "\033[m"
                    py = y
                    nx = x + wc
                case _:
                    # TODO: log errors and continue; MAYBE store errs in list and return them
                    # TODO: visually render some read bars in their assumed locations,
                    #   derived on ordered list of prev/next tokens
                    assert_never(token)
        if l:
            strings.append(l)
        return strings

    def tui_bake_display_area(self, handle: str, va: VisibleArea) -> DisplayList:
        # pylint:disable=too-many-locals,too-many-branches

        # BET: pylint:disable=fixme
        #   [_] use list of layers, ordered by zi
        #   [_] add optional check for non-overlapping inside each layer
        #   [_] use list/tuple of tokens per visual line -- to explore them in miur easier
        displ: DisplayList = []

        i = va.beg_cursor
        lenitems = self.view_len_items(handle)
        # ATT:(cy<vph):TEMP:RND: always draw full list of spacers to override trash
        cy = va.vp_y
        while i < lenitems and cy < va.vp_h:
            cx = va.vp_x

            # XLR: how to chain this better
            def unfit(ss: str, wc: int = 0) -> bool:
                nonlocal cx
                sw = wc or width(ss)
                if cx + sw > va.vp_w:
                    return True
                displ.append(TextSpan(cx, cy, ss, sw))
                cx += sw
                return False

            if unfit(f"{cy + 1:02d}:"):
                break

            # PERF? merge multiple tokens with same style into continuous spans
            #   BUT:BAD? mouse-click and diff-update will be much more messy?
            while i < lenitems:
                okcx = cx
                oklen = len(displ)
                # CHG?(" " * 1): use Spacer(1) ?
                if unfit(" " * 1) or unfit(f"{i:02d}:"):
                    cx = okcx
                    del displ[oklen:]
                    break

                # item = items[i]
                item = self.view_get_item(handle, i)
                text = make_printable(item.text)
                tw = width(text)
                if cx + tw > va.vp_w:
                    cx = okcx
                    del displ[oklen:]
                    break
                ## ALT:(no hi): displ.append(TextSpan(cx, cy, text, tw)); cx += tw
                cx = self.tui_enrich_hi(displ, text, cx, cy)

                i += 1
                break  # TEMP: process one item per line
            if va.vp_w < va.wnd_w:
                self.tui_pad_boundary(displ, cx, cy, va.vp_w, va.wnd_w)
            cy += 1
        va.end_actual = i
        return displ

    def tui_enrich_hi(self, displ: DisplayList, text: str, cx: int, cy: int) -> int:
        hipatt = r"[._-]"
        pe = 0
        for m in re.finditer(hipatt, text):
            if m.start() > pe:
                ab = text[pe : m.start()]
                abw = width(ab)
                displ.append(TextSpan(cx, cy, ab, abw))
                cx += abw
            needle = m.group()
            ndw = width(needle)
            sid = hipatt.index(needle)  # TEMP:HACK: diff style
            displ.append(TextSpan(cx, cy, needle, ndw, sid=sid))
            cx += ndw
            pe = m.end()
        if pe < len(text):
            ab = text[pe:]
            abw = width(ab)
            displ.append(TextSpan(cx, cy, ab, abw))
            cx += abw
        return cx

    def tui_pad_boundary(  # pylint:disable=too-many-arguments,too-many-positional-arguments
        self, displ: DisplayList, cx: int, cy: int, vpw: int, tww: int
    ) -> int:
        boundary = "|"  # ALT="|\n↪"
        bounw = width(boundary)
        spacer = vpw - cx
        if spacer > 0:
            # ALT:(string): l = wcwidth.ljust(l, vpw, " ") + "|"
            displ.append(TextSpan(cx, cy, " " * spacer, spacer))
            cx += spacer
            displ.append(TextSpan(cx, cy, boundary, bounw))
        elif spacer == 0:
            displ.append(TextSpan(cx, cy, boundary, bounw))
        else:
            # WARN: list needs to be sorted by .x
            # ALT:PERF: for large lists use bisect()
            broken_token_idx = -1
            for i in range(len(displ) - 1, -1, -1):
                t = displ[i]
                if t.y != cy:
                    break  # <CASE: single line only
                # FAIL:FIXME: splice if exactly bw two tokens
                if t.x <= vpw < t.x + t.wc:
                    broken_token_idx = i
                    break  # <FIXME? also split multiple overlapping tokens
            if broken_token_idx != -1:
                t = displ[broken_token_idx]
                # FAIL:(only works on ascii): l = l[:vpw] + "|" + l[vpw:]
                a = clip(t.t, 0, vpw - t.x, fillchar="·")
                b = clip(t.t, vpw - t.x, tww - vpw, fillchar="·")
                wa = width(a)
                displ[broken_token_idx : broken_token_idx + 1] = [
                    t._replace(t=a, wc=wa),
                    TextSpan(t.x + wa, t.y, boundary, bounw),
                    t._replace(t=b, wc=width(b)),
                ]
        return cx


class UI:
    def redraw(self, kernel: MiurKernel, handle: str, va: VisibleArea) -> str:
        t0 = monotonic_ns()
        va.wnd_w, va.wnd_h = shutil.get_terminal_size(fallback=(80, 24))
        va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, va.wnd_h)
        displ = kernel.tui_bake_display_area(handle, va)
        strings = kernel.tui_render_term_strings(displ)
        t1 = monotonic_ns()
        # NOTE: separate print() delays from frame preps measurement
        mydrv_print = print
        mydrv_print("".join(strings))
        t2 = monotonic_ns()
        return f"dt={(t1 - t0) / 1e6:.3f}ms [+{(t2 - t1) / 1e6:.3f}ms] (tokens={len(displ)})"


def main() -> str | None:
    try:
        perf: list[str] = []
        k = MiurKernel()
        h = "/data/g/miur_gen/demo/errors/chained.py"
        va = VisibleArea(4, 11)
        perf.append(UI().redraw(k, h, va))

        # handle = "/etc"
        # names = list(sorted(k.lfs_listdir(handle)))
        # perf.append(UI().redraw(names))
        #
        # focused = next(nm for nm in names if nm.startswith("av"))  # TEMP
        #
        # subhdl = handle + "/" + focused
        # subdir = list(sorted(k.lfs_listdir(subhdl)))
        # UI().redraw(subdir)
        # perf.append(UI().redraw(subdir))
        perfstr = " | ".join(perf)
        if "jurigged" in __import__("sys").modules:
            return perfstr
        print(perfstr)
        return None

    except Exception:
        from rich.traceback import install  # PERF:BAD: +400ms

        install(show_locals=True)
        raise
