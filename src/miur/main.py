import os
import re
import shutil
from time import monotonic_ns
from typing import NamedTuple, assert_never

## PERF: 45ms(bare) -> 75ms(typing) -> 95ms(shutil,time) -> 250ms(wcwidth)
# ALT:(copy-paste): //site-packages/_pytest/_io/wcwidth.py
from wcwidth import clip, width

# from typing import Iterator

type DisplayList = list[TextSpan]


class TextSpan(NamedTuple):
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


class MiurKernel:
    # def exec(self, op: object) -> object:
    #     return None
    def lfs_listdir(self, h: str) -> list[str]:
        with os.scandir(h) as it:
            return [x.name for x in it]


class UI:
    def redraw(self, names: list[str]) -> None:
        t0 = monotonic_ns()
        displ = self.bake(names)
        self.render(displ)
        t1 = monotonic_ns()
        print(f"[dt={(t1 - t0) / 1e6:.3f}ms (tokens={len(displ)})]")

    def render(self, displ: DisplayList) -> None:
        mydrv_print = print
        py = 0
        nx = 0
        l = ""
        for token in displ:
            match token:
                case TextSpan(x, y, text, wc, sid):
                    # DEBUG: mydrv_print(x, y, text, wc, sid)
                    # print(token, nx)
                    if nl := y - py:
                        assert nl > 0
                        mydrv_print(l + "\n" * nl, end="")
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
            mydrv_print(l)

    def bake(self, names: list[str]) -> DisplayList:  # pylint:disable=too-many-locals
        hipatt = r"[._-]"
        boundary = "|"
        bounw = width(boundary)
        tww, twh = shutil.get_terminal_size(fallback=(80, 24))
        vpw, vph = min(100, tww), min(3, twh)
        i = 0
        ch = 0
        lenitems = len(names)
        displ: DisplayList = []

        # ATT:TEMP:RND: always draw full list of spacers to override trash
        while i < lenitems and ch < vph:
            pfx = f"{ch + 1:02d}:"
            pw = width(pfx)
            displ.append(TextSpan(0, ch, pfx, pw))
            # PERF? merge multiple tokens with same style into continuous spans
            #   BUT:BAD? mouse-click and diff-update will be much more messy?
            lw = pw
            while i < lenitems:
                sw = 1
                if lw + sw > vpw:
                    break
                displ.append(TextSpan(lw, ch, " " * sw, sw))  # CHG? use Spacer(1) ?
                lw += sw

                text = names[i]
                tw = width(text)
                if lw + tw > vpw:
                    break

                ## ALT: no hi
                # displ.append(TextSpan(lw, ch, text, tw))
                # lw += tw
                pe = 0
                for m in re.finditer(hipatt, text):
                    if m.start() > pe:
                        ab = text[pe : m.start()]
                        abw = width(ab)
                        displ.append(TextSpan(lw, ch, ab, abw))
                        lw += abw
                    needle = m.group()
                    ndw = width(needle)
                    sid = hipatt.index(needle)  # TEMP:HACK: diff style
                    displ.append(TextSpan(lw, ch, needle, ndw, sid=sid))
                    lw += ndw
                    pe = m.end()
                if pe < len(text):
                    ab = text[pe:]
                    abw = width(ab)
                    displ.append(TextSpan(lw, ch, ab, abw))
                    lw += abw

                i += 1
            spacer = vpw - lw
            if spacer > 0:
                # ALT:(string): l = wcwidth.ljust(l, vpw, " ") + "|"
                displ.append(TextSpan(lw, ch, " " * spacer, spacer))
                lw += spacer
                displ.append(TextSpan(lw, ch, boundary, bounw))
            elif spacer == 0:
                displ.append(TextSpan(lw, ch, boundary, bounw))
            else:
                # WARN: list needs to be sorted by .x
                # ALT:PERF: for large lists use bisect()
                broken_token_idx = -1
                for i in range(len(displ) - 1, -1, -1):
                    t = displ[i]
                    if t.y != ch:
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
            ch += 1
        return displ


def main() -> None:
    try:
        k = MiurKernel()
        handle = "/etc"
        names = list(sorted(k.lfs_listdir(handle)))
        UI().redraw(names)
        focused = next(nm for nm in names if nm.startswith("av"))
        subhdl = handle + "/" + focused
        subdir = list(sorted(k.lfs_listdir(subhdl)))
        UI().redraw(subdir)
    except Exception:
        ## PERF:BAD: +400ms
        # from rich.traceback import install
        #
        # install(show_locals=True)
        raise
