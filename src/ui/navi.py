from typing import Iterable, Iterator, Self, override

import _curses as C

from ..alg.flowratio import flowratio_to_abs
from ..entity.base import Entity
from ..entity.error import ErrorEntry
from ..entity.rootnode import RootNode
from ..util.logger import log
from .colorscheme import g_style as S
from .navihistory import EntityViewCachePool, HistoryCursor
from .rect import Rect
from .view import EntityView


class Panel:
    _rect: Rect

    # RENAME?(space_alloci):COS:BAD:(ratio): contains fixed/flexed panel size, inof rational fraction
    def __init__(
        self,
        name: str = "",
        split: list[Self] | None = None,
        *,
        w: int | float = 0,
        h: int | float = 0,
        sepw: int = 0,
        # visible: bool = True,
    ) -> None:
        self.name = name
        self._hint_wh = (w, h)
        # self.size = size
        self._split = split if split is not None else []
        # self._visible = visible  # RENAME? enabled,present
        self._sepw = sepw

    @property
    def rect(self) -> Rect:
        return self._rect

    @override
    def __repr__(self) -> str:
        return f"{self.name}{self._rect}:({", ".join(map(repr,self._split))})"

    def __len__(self) -> int:
        return len(self._split)

    def __iter__(self) -> Iterator[Self]:
        return iter(self._split)

    def __getitem__(self, nm: str) -> Self | None:
        if nm == self.name:
            return self
        return next((p for p in self._split if p[nm] is not None), None)

    # def __contains__(self, nm: str) -> bool:
    #     if nm == self.name:
    #         return True
    #     return any(nm in p for p in self._split)

    def named_rects(self) -> Iterable[tuple[str, Rect]]:
        for p in self._split:
            if nm := p.name:
                yield (nm, p._rect)
            yield from p.named_rects()

    def sep_rects(self) -> Iterable[tuple[str, Rect]]:
        assert hasattr(self, "_rect"), "Err: call .resize() beforehand"
        pr: Rect | None = None
        for p in self._split:
            # NOTE: go from left to right
            yield from p.sep_rects()
            r = p.rect
            if pr:
                yield (p.name, Rect(w=r.x - pr.xw, h=pr.h, x=pr.xw, y=pr.y))
            pr = r

    def resize(self, maxrect: Rect) -> None:
        # NOTE: _hint_wh is only used by its parent and otherwise ignored
        self._rect = maxrect
        if not self._split:
            return
        # TODO? incorporate .visible=0/1 to affect flexed area
        # RND:TEMP: hardcoded=VSplit; keep vh=0 to stretch to full Rect.height
        assert all(x._hint_wh[1] == 0 for x in self._split)
        ratio = [x._hint_wh[0] for x in self._split]
        allsepw = self._sepw * (len(ratio) - 1)
        ws = flowratio_to_abs(ratio, maxrect.w - allsepw)
        vx = maxrect.x
        for p, w in zip(self._split, ws):
            p.resize(Rect(w, maxrect.h, x=vx, y=maxrect.y))
            # INFO: we don't need spacer column after rightmost vlst
            #   >> it should be a .frame or a .margin
            vx += w + self._sepw


# NOTE:(`AdaptiveLayout): change/hide based on total window size
#   DFL:(prio): browser:0 -> preview:1 -> parent:2 [-> pparent:3]
# TODO: toggle/limit linewrap + content-awareness
# TODO: header/footer hide/xfm
def pick_adaptive_layout_cfg(rect: Rect, old: Panel, sepw: int) -> Panel:
    if rect.w < 30:
        if old.name == "navi_vlst":
            return old
        browse = Panel("browse", [Panel("tab0")])
        return Panel("navi_vlst", [browse])

    if rect.w < 45:
        if old.name == "navi_pv0":
            return old
        browse = Panel("browse", [Panel("tab0")])
        preview = Panel("preview", [Panel("pv0")], w=12)
        return Panel("navi_pv0", [browse, preview])

    if rect.w < 60:
        if old.name == "navi_miller0":
            return old
        prevloci = Panel("prevloci", [Panel("prev")], w=8)
        browse = Panel("browse", [Panel("tab0")])
        preview = Panel("preview", [Panel("pv0")], w=12)
        return Panel("navi_miller0", [prevloci, browse, preview], sepw=sepw)

    if rect.w < 70:
        if old.name == "navi_miller1":
            return old
        prevloci = Panel("prevloci", [Panel("prev")], w=16, sepw=sepw)
        browse = Panel("browse", [Panel("tab0")], w=0.5, sepw=sepw)
        preview = Panel("preview", [Panel("pv0")], sepw=sepw)
        return Panel("navi_miller1", [prevloci, browse, preview], sepw=sepw)

    if old.name == "navi_miller2":
        return old
    prevloci = Panel(
        "prevloci", [Panel("pprev", w=0.4), Panel("prev")], w=22, sepw=sepw
    )
    browse = Panel("browse", [Panel("tab0")], w=0.5, sepw=sepw)
    # interp = PanelView(ratio=(0,))
    preview = Panel("preview", [Panel("pv0", w=0.7), Panel("pv1")], sepw=sepw)
    # TODO:ALSO: return linewrap/header/footer cfg overrides
    return Panel("navi_miller2", [prevloci, browse, preview], sepw=sepw)


# SPLIT:(`NaviLayout): to cvt {(ww,wh) -> [panel].rect}, adapt based on size and toggle visibility
class NaviWidget:
    def __init__(self, ent: Entity) -> None:
        self._pool = EntityViewCachePool()
        rootnode = ent if isinstance(ent, RootNode) else RootNode()
        self._hist = HistoryCursor(rootnode, self._pool)
        self._hist.jump_to(self._view._wdg.focused_item._ent)
        # self._hist.jump_to(rootnode._vlst[0])
        # self._hist.jump_to(ent, intermediates=True)
        self._layout = Panel()
        self._colsep = ""  # "│"  # OR=█|┃│ OR=<Space>

    # PERF?IDEA: use @cached and reset by "del self._view" in "view_go_*()"
    # RENAME:(view) make it publicly accessible from keymap:lambda
    @property
    def _view(self) -> EntityView:
        return self._hist.focused_view

    ## ALT:(rect/origin): do C.move(y,x) b4 .redraw(), and remember getyx() inside each .redraw()
    def resize(self, vh: int, vw: int, orig_yx: tuple[int, int] = (0, 0)) -> None:
        rect = Rect(vw, vh, x=orig_yx[1], y=orig_yx[0])
        self._layout = pick_adaptive_layout_cfg(
            rect,
            old=self._layout,
            sepw=len(self._colsep),  # FIXME: len() -> cellwidth()
        )
        self._layout.resize(rect)
        log.debug(f"{self._layout=}")  # <TEMP:DEBUG
        # WHY: adaptive layout on bigger window may need more preview nodes
        self._update_preview()
        self._resize_cached_preview()
        self._resize_cached_hist_browse()

    def cursor_jump_to(self, idx: int) -> None:
        self._view._wdg.focus_on(idx)  # pylint:disable=protected-access
        self._update_preview()
        self._resize_cached_preview()

    def cursor_step_by(self, steps: int) -> None:
        self._view._wdg.step_by(steps)  # pylint:disable=protected-access
        self._update_preview()
        self._resize_cached_preview()

    def view_go_into(self) -> None:
        # pylint:disable=protected-access
        pwdg = self._view._wdg
        if not pwdg._lst:
            log.trace("<<EMPTY>>")
            return
        self.view_jump_to(pwdg.focused_item._ent)

    def view_jump_to(self, nent: Entity) -> None:
        self._hist.jump_to(nent)
        self._update_preview()
        self._resize_cached_preview()
        self._resize_cached_hist_browse()

    def view_go_back(self) -> None:
        self._hist.go_back()
        # WHY: after previous jump_to() we may return to disjoint parent with different preview()
        self._update_preview()
        self._resize_cached_preview()
        # WHY: forced resize() old/cached wdg, as window may had resized from then.
        self._resize_cached_hist_browse()

    def _update_preview(self) -> None:
        if not (pvs := self._layout["preview"]):
            return
        # pylint:disable=protected-access
        wdg = self._hist.focused_view._wdg
        # [_] RFC: isolate same ALG of traversing list of `Panels for _resize_cached*(), etc.
        for _ in pvs:
            if not wdg._lst:
                break
            cent = wdg.focused_item._ent
            if isinstance(cent, ErrorEntry):
                break  # TEMP: until I make errors explorable
            # NOTE: directly draw "preview" panel/entity from _pool
            #   &why to avoid constantly rewriting history on each cursor move up/down
            peek = self._pool.get(cent)
            if not peek:
                peek = self._pool.add(cent)
            wdg = peek._wdg

    def _resize_cached_preview(self) -> None:
        if not (pvs := self._layout["preview"]):
            return
        # ALT:HACK: clone rect size from old.preview
        #   BUT:FAIL: on startup there is yet no "old.preview" nor initial .resize()
        #   wdg.resize(*self._preview._wdg.sizehw)
        roomw = pvs.rect.w
        wdg = self._hist.focused_view._wdg
        for p in pvs:
            r = p.rect
            if not wdg._lst:
                break
            cent = wdg.focused_item._ent
            peek = self._pool.get(cent)
            if not peek:
                break  # COS: consequent previews are depending on previous ones
            wdg = peek._wdg
            # BAD: too brittle and hard to trace the flow; BET:ENH: `AdaptiveLayout
            #   ALSO: expand browse= over prevloci/preview areas when there is none
            #   OR: dynamically give more preview area for TextSyntax files and less for browse=
            ## NOTE: if `Error is inside preview= pv0 -- we can extend it over empty pv1
            haspv1 = wdg._lst and not isinstance(wdg.focused_item._ent, ErrorEntry)
            w = r.w if haspv1 else roomw
            wdg.resize(r.h, w, origin=(r.y, r.x))
            roomw -= w + pvs._sepw

    def _resize_cached_hist_browse(self) -> None:
        if plocs := self._layout["prevloci"]:
            pr: Rect | None = None
            for i, p in enumerate(plocs, start=-len(plocs)):
                r = p.rect
                if prev := self._hist.get_relative(i):
                    prev._wdg.resize(r.h, r.w, origin=(r.y, r.x))
                ## BAD: too brittle and hard to trace the flow; BET:ENH: `AdaptiveLayout
                ## BUG: corrupts colsep after being triggered even once
                # r = p.rect
                # if pr is None:
                #     pr = r
                # if prev := self._hist.get_relative(i):
                #     prev._wdg.resize(pr.h, pr.w, origin=(pr.y, pr.x))
                #     pr = None
                # else:
                #     # NOTE:(for N=2): extend first hist.w when len(hist)<len(prevloci)
                #     #   FIXME:(for N>=3): re-balance same vw bw lower number of hist nodes
                #     pr.w += r.w + plocs._sepw

        # MAYBE: extend browse= to whole hist/preview when hist=none or preview=none
        if browse := self._layout["browse"]:
            for p in browse:
                r = p.rect
                self._view._wdg.resize(r.h, r.w, origin=(r.y, r.x))

    def redraw(self, stdscr: C.window) -> tuple[int, int]:
        # pylint:disable=protected-access
        if plocs := self._layout["prevloci"]:
            for i, p in enumerate(plocs, start=-len(plocs)):
                if prev := self._hist.get_relative(i):
                    log.trace(p.name)
                    prev._wdg.redraw(stdscr, numcol=False)

        if pvs := self._layout["preview"]:
            wdg = self._hist.focused_view._wdg
            for p in pvs:
                if not wdg._lst:
                    break
                peek = self._pool.get(wdg.focused_item._ent)
                if not peek:
                    break  # COS: consequent previews are depending on previous ones
                wdg = peek._wdg
                log.trace(p.name)
                wdg.redraw(stdscr, numcol=False)

        # NOTE: draw main Browse column very last to always be on top
        curyx = (0, 0)
        if browse := self._layout["browse"]:
            for p in browse:
                log.trace(p.name)
                curyx = self._view._wdg.redraw(stdscr, numcol=True)

        # NOTE: spacer definitely belongs to `Navi, as it's in-between vlst`s
        #   ALSO: it should be drawn in one go even if there is only 1 item (and for each line of multiline item)
        if colsep := self._colsep:
            sattr = S.iteminfo
            for nm, sr in self._layout.sep_rects():
                x, w = sr.x, sr.w
                for y in range(sr.y, sr.yh):
                    stdscr.addnstr(y, x, colsep, w, sattr)

        # TODO: return curyx from focused panel inof the last one on the right
        return curyx
