from typing import Iterable, Iterator, Self, override

import _curses as C

from ..alg.flowratio import flowratio_to_abs
from ..entity.base.golden import Entity
from ..entity.core.error import ErrorEntry
from ..entity.core.rootnode import RootNode
from ..util.logger import log
from .colorscheme import g_style as S
from .navihistory import EntityViewCachePool, HistoryCursor
from .panel import Panel
from .panelcfg import PanelCfg
from .rect import Rect
from .view import EntityView


class NaviWidget:
    def __init__(self, rootnode: Entity) -> None:
        self._pool = EntityViewCachePool()
        ## RND:CHG:DONE: instead of building path "root..ent" we make "ent" a new "[fake]root"
        #   and isolate navi to be inside it OR reroot/reroute to parents only by ".."
        # rootnode = ent if isinstance(ent, RootNode) else RootNode()
        # ...
        # self._hist.jump_to(ent, intermediates=True)
        self._hist = HistoryCursor(rootnode, self._pool)
        # TEMP:RND: imm jump to contents of RootNode.explore()
        self._hist.jump_to(self._view._wdg.focused_item._ent)
        self._layout = Panel()
        self._colsep = ""  # "│"  # OR=█|┃│ OR=<Space>
        self._layoutstrategy = "adaptive"

    # SPLIT:(`NaviLayout): to cvt {(ww,wh) -> [panel].rect}, adapt based on size and toggle visibility
    def set_layout(self, strategy: str, rect: Rect | None = None) -> None:
        if rect is None:
            rect = self._layout.rect
        # TODO: toggle/limit linewrap + content-awareness
        # TODO: header/footer hide/xfm
        pcfg = PanelCfg(sepw=len(self._colsep))  # FIXME: len() -> cellwidth()
        mk = getattr(pcfg, "navi_" + strategy)
        if mk.__name__.endswith("adaptive"):
            self._layout = mk(rect, old=self._layout)
        else:
            self._layout = mk()
        self._layoutstrategy = strategy
        self._layout.resize(rect)
        log.debug(f"{strategy}: {self._layout=}")  # <TEMP:DEBUG

        # WHY: adaptive layout on bigger window may need more preview nodes
        self._update_preview()
        self._resize_cached_preview()
        self._resize_cached_hist_browse()

    # PERF?IDEA: use @cached and reset by "del self._view" in "view_go_*()"
    # RENAME:(view) make it publicly accessible from keymap:lambda
    @property
    def _view(self) -> EntityView:
        return self._hist.focused_view

    ## ALT:(rect/origin): do C.move(y,x) b4 .redraw(), and remember getyx() inside each .redraw()
    def resize(self, vh: int, vw: int, orig_yx: tuple[int, int] = (0, 0)) -> None:
        rect = Rect(vw, vh, x=orig_yx[1], y=orig_yx[0])
        self.set_layout(self._layoutstrategy, rect)

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
                # HACK: prevent auto-launching of @demo apps by preview (press <L>)
                if not getattr(cent, "allowpreview", True):
                    break
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
                    # log.trace(p.name)
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
                # log.trace(p.name)
                wdg.redraw(stdscr, numcol=False)

        # NOTE: draw main Browse column very last to always be on top
        curyx = (0, 0)
        if browse := self._layout["browse"]:
            for p in browse:
                # log.trace(p.name)
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
