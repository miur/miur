from typing import Iterable, Iterator, Self, override

import _curses as C

from ..alg.flowratio import flowratio_to_abs
from ..util.logger import log
from .entity_base import Golden
from .entries import ErrorEntry, RootNode
from .navihistory import EntityViewCachePool, HistoryCursor
from .rect import Rect
from .view import EntityView


class Panel:
    # RENAME?(space_alloci):COS:BAD:(ratio): contains fixed/flexed panel size, inof rational fraction
    def __init__(
        self,
        name: str = "",
        split: list[Self] | None = None,
        *,
        w: int | float = 0,
        h: int | float = 0,
        # visible: bool = True,
    ) -> None:
        self.name = name
        self._hint_wh = (w, h)
        # self.size = size
        self._split = split if split is not None else []
        # self._visible = visible  # RENAME? enabled,present
        self._rect: Rect

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

    def resize(self, maxrect: Rect) -> None:
        # NOTE: _hint_wh is only used by its parent and otherwise ignored
        self._rect = maxrect
        if not self._split:
            return
        # TODO: incorporate .visible=0/1 to affect flexed area
        # TEMP: hardcoded=VSplit; keep vh=0 to stretch to full Rect.height
        assert all(x._hint_wh[1] == 0 for x in self._split)
        ratio = [x._hint_wh[0] for x in self._split]
        ws = flowratio_to_abs(ratio, maxrect.w)
        vx = maxrect.x
        for p, w in zip(self._split, ws):
            p.resize(Rect(w, maxrect.h, x=vx, y=maxrect.y))
            vx += w


# NOTE:(`AdaptiveLayout): change/hide based on total window size
#   DFL:(prio): browser:0 -> preview:1 -> parent:2 [-> pparent:3]
# TODO: toggle/limit linewrap + content-awareness
# TODO: header/footer hide/xfm
def pick_adaptive_layout_cfg(rect: Rect, old: Panel) -> Panel:
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
        return Panel("navi_miller0", [prevloci, browse, preview])

    if rect.w < 70:
        if old.name == "navi_miller1":
            return old
        prevloci = Panel("prevloci", [Panel("prev")], w=16)
        browse = Panel("browse", [Panel("tab0")], w=0.5)
        preview = Panel("preview", [Panel("pv0")])
        return Panel("navi_miller1", [prevloci, browse, preview])

    if old.name == "navi_miller2":
        return old
    prevloci = Panel("prevloci", [Panel("pprev", w=0.4), Panel("prev")], w=22)
    browse = Panel("browse", [Panel("tab0")], w=0.5)
    # interp = PanelView(ratio=(0,))
    preview = Panel("preview", [Panel("pv0", w=0.7), Panel("pv1")])
    # TODO:ALSO: return linewrap/header/footer cfg overrides
    return Panel("navi_miller2", [prevloci, browse, preview])


# SPLIT:(`NaviLayout): to cvt {(ww,wh) -> [panel].rect}, adapt based on size and toggle visibility
class NaviWidget:
    def __init__(self, ent: Golden) -> None:
        self._pool = EntityViewCachePool()
        self._hist = HistoryCursor(RootNode(), self._pool)
        self._hist.jump_to(ent, intermediates=True)
        self._layout = Panel()

    # PERF?IDEA: use @cached and reset by "del self._view" in "view_go_*()"
    # RENAME:(view) make it publicly accessible from keymap:lambda
    @property
    def _view(self) -> EntityView:
        return self._hist.focused_view

    ## ALT:(rect/origin): do C.move(y,x) b4 .redraw(), and remember getyx() inside each .redraw()
    def resize(self, vh: int, vw: int, orig_yx: tuple[int, int] = (0, 0)) -> None:
        rect = Rect(vw, vh, x=orig_yx[1], y=orig_yx[0])
        self._layout = pick_adaptive_layout_cfg(rect, self._layout)
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

    def view_jump_to(self, nent: Golden) -> None:
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
        rects = [p.rect for p in pvs]
        roomw = sum(r.w for r in rects)
        wdg = self._hist.focused_view._wdg
        for r in rects:
            if not wdg._lst:
                break
            cent = wdg.focused_item._ent
            peek = self._pool.get(cent)
            if not peek:
                break  # COS: consequent previews are depending on previous ones
            wdg = peek._wdg
            ## NOTE: if `Error is inside preview= pv0 -- we can extend it over empty pv1
            haspv1 = wdg._lst and not isinstance(wdg.focused_item._ent, ErrorEntry)
            w = r.w if haspv1 else roomw
            wdg.resize(r.h, w, origin=(r.y, r.x))
            roomw -= w

    def _resize_cached_hist_browse(self) -> None:
        if plocs := self._layout["prevloci"]:
            rects = [p.rect for p in plocs]
            pr: Rect | None = None
            for i, r in enumerate(rects, start=-len(rects)):
                if pr is None:
                    pr = r
                else:
                    # NOTE:(for N=2): extend first hist.w when len(hist)<len(prevloci)
                    #   FIXME:(for N>=3): re-balance same vw bw lower number of hist nodes
                    pr.w += r.w
                if prev := self._hist.get_relative(i):
                    prev._wdg.resize(pr.h, pr.w, origin=(pr.y, pr.x))
                    pr = None

        # MAYBE: extend browse= to whole hist/preview when hist=none or preview=none
        if browse := self._layout["browse"]:
            for p in browse:
                r = p.rect
                self._view._wdg.resize(r.h, r.w, origin=(r.y, r.x))

    def redraw(self, stdscr: C.window) -> tuple[int, int]:
        # pylint:disable=protected-access
        if plocs := self._layout["prevloci"]:
            for i, _ in enumerate(plocs, start=-len(plocs)):
                if prev := self._hist.get_relative(i):
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
                # NOTE: we don't need spacer column after rightmost vlst
                colsep = p.rect.xw < self._layout.rect.xw
                wdg.redraw(stdscr, numcol=False, colsep=colsep)

        # NOTE: draw main Browse column very last to always be on top
        curyx = (0, 0)
        if browse := self._layout["browse"]:
            for _ in browse:
                curyx = self._view._wdg.redraw(stdscr, numcol=True)
        # TODO: return curyx from focused panel inof the last one on the right
        return curyx
