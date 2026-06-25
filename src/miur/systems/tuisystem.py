import os
import re
from dataclasses import dataclass
from typing import TYPE_CHECKING, Protocol

## PERF:(startup): 45ms(bare) -> 75ms(typing) -> 95ms(shutil,time) -> 250ms(wcwidth)
# ALT:(copy-paste): //site-packages/_pytest/_io/wcwidth.py
from wcwidth import width

from ..uicommon.displaylist import DisplayList, TextSpan
from ..uicommon.styleids import Aid, StyleId

if TYPE_CHECKING:
    from .viewsystem import ViewSystem

    class IKernel(Protocol):
        view: ViewSystem


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


def make_printable(text: str) -> str:
    # VIZ: https://en.wikipedia.org/wiki/List_of_Unicode_characters#Control_Pictures
    # text = text.rstrip("\r\n") or " "
    text = text.replace(os.linesep, "␤")  # "↵¬"
    # text = text.replace("\t", "▸ ").replace("\x7f", "␡").replace(" ", "␣")
    NUL = ord("␀")
    SP = ord(" ")
    return "".join(chr(NUL + ci) if (ci := ord(c)) < SP else c for c in text)


# RENAME? cellgriduisystem
class TuiSystem:
    __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel

    def bake_display_area(self, handle: str, va: VisibleArea) -> DisplayList:
        # pylint:disable=too-many-locals,too-many-branches

        # BET: pylint:disable=fixme
        #   [_] use list of layers, ordered by zi
        #   [_] add optional check for non-overlapping inside each layer
        #   [_] use list/tuple of tokens per visual line -- to explore them in miur easier
        displ: DisplayList = []

        i = va.beg_cursor
        lenitems = self.k.view.len_items(handle)
        # ATT:(cy<vph):TEMP:RND: always draw full list of spacers to override trash
        cy = va.vp_y
        while i < lenitems and cy < va.vp_h:
            cx = va.vp_x

            # XLR: how to chain this better
            def unfit(
                ss: str, wc: int = 0, aid: StyleId = Aid.DEFAULT, cy: int = cy
            ) -> bool:
                nonlocal cx
                sw = wc or width(ss)
                if cx + sw > va.vp_w:
                    return True
                displ.append(TextSpan(cx, cy, ss, sw, aid))
                cx += sw
                return False

            if unfit(f"{cy + 1:02d}:", aid=Aid.LINEIDX):
                break

            # PERF? merge multiple tokens with same style into continuous spans
            #   BUT:BAD? mouse-click and diff-update will be much more messy?
            while i < lenitems:
                okcx = cx
                oklen = len(displ)
                # CHG?(" " * 1): use Spacer(1) ?
                if unfit(" " * 1) or unfit(f"{i:02d}:", aid=Aid.ITEMIDX):
                    cx = okcx
                    del displ[oklen:]
                    break

                # item = items[i]
                item = self.k.view.get_item(handle, i)
                text = make_printable(item.text)
                tw = width(text)
                if cx + tw > va.vp_w:
                    cx = okcx
                    del displ[oklen:]
                    break
                ## ALT:(no hi): displ.append(TextSpan(cx, cy, text, tw)); cx += tw
                cx = self.hi_match(displ, text, cx, cy)

                i += 1
                break  # TEMP: process one item per line
            cy += 1
        va.end_actual = i
        return displ

    # CHG: hi match text/chars into bold RED
    #   >> dim/underline hi! after pressing <CR/Esc> to reduce hi! distraction during navi over filtered
    def hi_match(self, displ: DisplayList, text: str, cx: int, cy: int) -> int:
        hipatt = r"[._-]"
        pe = 0
        for m in re.finditer(hipatt, text):
            if m.start() > pe:
                ab = text[pe : m.start()]
                abw = width(ab)
                displ.append(TextSpan(cx, cy, ab, abw, aid=Aid.ITEM))
                cx += abw
            needle = m.group()
            ndw = width(needle)
            displ.append(TextSpan(cx, cy, needle, ndw, aid=Aid.ITEMPUNCT))
            cx += ndw
            pe = m.end()
        if pe < len(text):
            ab = text[pe:]
            abw = width(ab)
            displ.append(TextSpan(cx, cy, ab, abw, aid=Aid.ITEM))
            cx += abw
        return cx
