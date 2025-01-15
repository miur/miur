import _curses as C

from ..alg.flowratio import flowratio_to_abs
from ..util.logger import log
from .entity_base import Golden
from .navihistory import HistoryCursor
from .view import EntityView


class PanelView:
    # RENAME?(space_alloci):COS:BAD:(ratio): contains fixed/flexed panel size, inof rational fraction
    def __init__(self, ratio: tuple[int | float, ...], visible: bool = True) -> None:
        self._sizeratio = ratio
        self._visible = visible  # RENAME? enabled,present

    def sizes(self, maxw: int) -> list[int]:
        return flowratio_to_abs(self._sizeratio, maxw)


# SPLIT:(`NaviLayout): to cvt {(ww,wh) -> [panel].rect}, adapt based on size and toggle visibility
class NaviWidget:
    def __init__(self, hist: HistoryCursor) -> None:
        self._hist = hist
        self._miller_ratio = (20, 0.4, 0)
        # self._miller_ratio = (12, 12, 12, 0.4, 0)
        self._view_rect = (0, 0, 0, 0)  # FMT:(vh,vw,vy,vx)

        # TODO:(`AdaptiveLayout): change/hide based on total window size
        #   DFL:(prio): browser:0 -> preview:1 -> parent:2 [-> pparent:3]
        self._cfg = PanelView(ratio=(24, 0.4, 0))
        self._cfg_prevloci = PanelView(ratio=(2, 3))
        self._cfg_browse = PanelView(ratio=(1,))
        # self._cfg_interp = PanelView(ratio=(1,))
        self._cfg_preview = PanelView(ratio=(1,))

    # PERF?IDEA: use @cached and reset by "del self._view" in "view_go_*()"
    # RENAME:(view) make it publicly accessible from keymap:lambda
    @property
    def _view(self) -> EntityView:
        return self._hist.focused_view

    @property
    def focused_ent(self) -> Golden:
        # pylint:disable=protected-access
        return self._hist.focused_view._wdg.focused_item._ent

    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        self._view_rect = vh, vw, *origin
        self._resize_layout(*self._view_rect)
        # BAD:AGAIN: we init very first preview() on very first resize event
        self._update_preview()

    def cursor_jump_to(self, idx: int) -> None:
        # pylint:disable=protected-access
        self._view._wdg.focus_on(idx)
        self._update_preview()

    def cursor_step_by(self, steps: int) -> None:
        # pylint:disable=protected-access
        self._view._wdg.step_by(steps)
        self._update_preview()

    def view_go_into(self) -> None:
        # pylint:disable=protected-access
        pwdg = self._view._wdg
        if not pwdg._lst:
            log.trace("<<EMPTY>>")
            return
        self.view_jump_to(pwdg.focused_item._ent)

    def view_jump_to(self, nent: Golden) -> None:
        self._hist.jump_to(nent)
        # NOTE: forced resize() *new* wdg to same dimensions as *pwdg*
        self._resize_layout(*self._view_rect)
        self._update_preview()

    # THINK:SPLIT: `NaviModel which knows when to yeild `RootNode (which holds rootfs/stdin/etc providers)
    def view_go_back(self) -> None:
        self._hist.go_back()
        # NOTE: forced resize() old/cached wdg, as window may had resized from then.
        self._resize_layout(*self._view_rect)
        self._update_preview()

    def _update_preview(self) -> EntityView:
        # pylint:disable=protected-access
        cent = self.focused_ent
        pool = self._hist._cache_pool
        view = pool.get(cent)
        if not view:
            view = pool[cent] = EntityView(cent)
            ## ALT:HACK: clone rect size from old.preview
            ##   FAIL: on startup there is no "old.preview"
            ##     BUT: you can't create one in "__init__" either
            ##       COS you need to wait until .resize() to calc abs-sz
            #   view._wdg.resize(*self._preview._wdg.sizehw)
            # BAD:PERF: we calc() preview= size AGAIN here (after _resize_layout)
            vh, vw, vy, vx = self._view_rect
            wlst = flowratio_to_abs(self._miller_ratio, vw)
            view._wdg.resize(vh, wlst[2], origin=(vy, vx + wlst[0] + wlst[1]))
        return view

    def _resize_layout(self, vh: int, vw: int, vy: int, vx: int) -> None:
        assert len(self._miller_ratio) == 3, "TEMP: until RFC explicit panels"
        wlst = flowratio_to_abs(self._miller_ratio, vw)
        log.info(f"{wlst=}")  # <TEMP:DEBUG

        # pylint:disable=protected-access
        if prev := self._hist.get_relative(-1):
            prev._wdg.resize(vh, wlst[0], origin=(vy, vx))
            # RND: we make leftmost column navigatable when we "collapse" to showing only RootNode
            #   CHECK:WF: is it intuitive or irritating ?
            vx += wlst[0]

        ## ALT:(origin): do C.move(y,x) b4 .redraw(), and remember getyx() inside each .redraw()
        self._view._wdg.resize(vh, wlst[1], origin=(vy, vx))
        vx += wlst[1]

        # CHG: use external shared `CachedEntities inof `HistoryCursor
        if peek := self._hist._cache_pool.get(self.focused_ent):
            peek._wdg.resize(vh, wlst[2], origin=(vy, vx))

    def redraw(self, stdscr: C.window) -> tuple[int, int]:
        # pylint:disable=protected-access
        if prev := self._hist.get_relative(-1):
            prev._wdg.redraw(stdscr, numcol=False)

        # CHG: use external shared `CachedEntities inof `HistoryCursor
        # NOTE: directly draw "preview" panel/entity from _pool
        #   &why to avoid constantly rewriting history on each cursor move up/down
        if peek := self._hist._cache_pool.get(self.focused_ent):
            peek._wdg.redraw(stdscr, numcol=False)

        # NOTE: draw main Browse column very last to always be on top
        return self._view._wdg.redraw(stdscr, numcol=True)
