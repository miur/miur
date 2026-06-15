import os
import re
from dataclasses import dataclass
from enum import IntEnum, auto
from typing import TYPE_CHECKING, Iterable, NamedTuple, Protocol

## PERF:(startup): 45ms(bare) -> 75ms(typing) -> 95ms(shutil,time) -> 250ms(wcwidth)
# ALT:(copy-paste): //site-packages/_pytest/_io/wcwidth.py
from wcwidth import width

if TYPE_CHECKING:
    from .viewsystem import ViewSystem

    class IKernel(Protocol):
        view: ViewSystem


## RND:(umbrella term): .aid = {attributes|algorithm|animation|appearance|applet}_id
#   WHY: short name .sid (static "style_id") is too similar to system_id
#   NICE: "aid" == "the aux means to accomplish smth" (e.g. rendering)
#   ALT:(name): style/effect/theme/look/presentation
#   ARCH: good fit for "presentation/appearance"
#     * signals a visual intent (inof "how to draw")
#     * allows composition of effects
#     * can apply transformation to content (like vertex shader)
#     * can describe transitioning algo (like animation)
#     * decorative animations and ops indicators
#       - blinking cursor OR plasma gradient bkgr
#       - bright dot on cursor boarder coursing around item text
#       - highlight one cell left-right-left-right as "indeterminate waiting"
#         ~~ e.g. for currenly downloaded partial files
#       - flash bg to indicate "copy/paste" operations
#         NEED: history to cvt to selection any "last flashed pasted selection"
#           or "touched/created files in folder"
#         ARCH: should be done fully on client side to minimize server/kernel latency for timestamps
#           ~~ ALSO: each UI may do stuff totally differently, so it may have no common parts on server
#             ALT: calc ani frame in kernel to get displ with static styles only on client
#       - WARN: diff-update won't work as both token and style are the same
#          >> NEED: store animations in some list to update those by timer
#     * allows gradients/beautifications
#       - e.g. enlarging font-size in Qt/GL or making a distorted zoom-bubble around cursor
#     * parametrized styles (e.g. color is based on some argvalue/function)
#       ? similar to "nested style" (TRY: generalize one to another)
#     * associates dif attrs to be used by diff UIs (e.g. RGB24 vs I16/I256 colors)
#       - ALSO: provides fg/bg styles for TUI; border for GUI; shape for opengl
#     * same id can mean dif things for obj-type (Text/Rect/Image/etc.)
#   IDEA: use each name fg/bg as progressbar for e.g. !mpd OR as gauge for e.g. relative file/dirsize like !ncdu
class Aid(IntEnum):
    unknown = 0  # ALT: py:$ def _generate_next_value_(name, start, count, last_values): return count
    default = auto()
    item = auto()
    itempunct = auto()
    lineidx = auto()
    itemidx = auto()
    footer = auto()


class TextSpan(NamedTuple):  # RENAME: CellSpan
    x: int
    y: int
    t: str
    wc: int  # <MAYBE? cache cell-width hint for renderer's BoundingBox
    aid: int  # WARN: don't use DFL=0 here -- always set style explicitly to .default
    zi: int = 1  # < Z-index of whole layer bucket (NOT per-element)
    ## ALT:
    # @property
    # def wc(self) -> int:
    #     return width(self.t)


type DisplayList = list[TextSpan]
type DisplayStream = Iterable[TextSpan]


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
            def unfit(ss: str, wc: int = 0, aid: Aid = Aid.default) -> bool:
                nonlocal cx
                sw = wc or width(ss)
                if cx + sw > va.vp_w:
                    return True
                displ.append(TextSpan(cx, cy, ss, sw, aid))
                cx += sw
                return False

            if unfit(f"{cy + 1:02d}:", aid=Aid.lineidx):
                break

            # PERF? merge multiple tokens with same style into continuous spans
            #   BUT:BAD? mouse-click and diff-update will be much more messy?
            while i < lenitems:
                okcx = cx
                oklen = len(displ)
                # CHG?(" " * 1): use Spacer(1) ?
                if unfit(" " * 1) or unfit(f"{i:02d}:", aid=Aid.itemidx):
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
                displ.append(TextSpan(cx, cy, ab, abw, aid=Aid.item))
                cx += abw
            needle = m.group()
            ndw = width(needle)
            # aid = hipatt.index(needle)  # TEMP:HACK: diff style
            aid = Aid.item
            displ.append(TextSpan(cx, cy, needle, ndw, aid=aid))
            cx += ndw
            pe = m.end()
        if pe < len(text):
            ab = text[pe:]
            abw = width(ab)
            displ.append(TextSpan(cx, cy, ab, abw, aid=Aid.item))
            cx += abw
        return cx
