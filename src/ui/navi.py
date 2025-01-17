import _curses as C

from ..alg.flowratio import flowratio_to_abs
from ..util.logger import log
from .entity_base import Golden
from .entries import ErrorEntry, RootNode
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
    def __init__(self, ent: Golden) -> None:
        self._hist = HistoryCursor(RootNode())
        self._hist.jump_to(ent, intermediates=True)
        self._view_rect = (0, 0, 0, 0)  # FMT:(vh,vw,vy,vx)

        # TODO:(`AdaptiveLayout): change/hide based on total window size
        #   DFL:(prio): browser:0 -> preview:1 -> parent:2 [-> pparent:3]
        self._cfg_layout = "prevloci browse preview".split()
        self._cfg = PanelView(ratio=(22, 0.5, 0))
        self._cfg_prevloci = PanelView(ratio=(0.4, 0))
        self._cfg_browse = PanelView(ratio=(0,))
        # self._cfg_interp = PanelView(ratio=(0,))
        self._cfg_preview = PanelView(ratio=(0.7, 0))

    # PERF?IDEA: use @cached and reset by "del self._view" in "view_go_*()"
    # RENAME:(view) make it publicly accessible from keymap:lambda
    @property
    def _view(self) -> EntityView:
        return self._hist.focused_view

    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        self._view_rect = vh, vw, *origin
        # TODO: incorporate .visible=0/1 to affect flexed area
        self._layout = {
            nm: getattr(self, "_cfg_" + nm).sizes(w)
            for i, w in enumerate(self._cfg.sizes(vw))
            if (nm := self._cfg_layout[i])
        }
        log.info(f"{self._layout=}")  # <TEMP:DEBUG

        # BAD:AGAIN: we init very first preview() on very first resize event
        # BAD: we can't discard .resize() from _update_preview, COS we need to do it for older nodes on <j/k>
        #   BUT:NICE: if we could discard it -- we could call _update_preview() from __init__ inof resize(),
        #     and then on startup first ever resize() would put proper sizes for all widgets
        #   SPLIT? make _resize_preview() function to call on <j/k> and achieve everything mentioned ?
        self._update_preview()
        self._resize_layout(*self._view_rect)

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
        self._update_preview()
        # NOTE: forced resize() *new* wdg to same dimensions as *pwdg*
        self._resize_layout(*self._view_rect)

    # THINK:SPLIT: `NaviModel which knows when to yeild `RootNode (which holds rootfs/stdin/etc providers)
    def view_go_back(self) -> None:
        self._hist.go_back()
        # WHY: after previous jump_to() we may return to disjoint parent with different preview()
        self._update_preview()
        # NOTE: forced resize() old/cached wdg, as window may had resized from then.
        self._resize_layout(*self._view_rect)

    # MAYBE? merge with _resize_layout(), as they always called together ?
    def _update_preview(self) -> None:
        # pylint:disable=protected-access
        vh, vw, vy, vx = self._view_rect
        pool = self._hist._cache_pool
        wdg = self._hist.focused_view._wdg
        vx += sum(self._layout["prevloci"]) + sum(self._layout["browse"])
        for w in self._layout["preview"]:
            if not wdg._lst:
                break
            cent = wdg.focused_item._ent
            if isinstance(cent, ErrorEntry):
                break  # TEMP: until I make errors explorable
            peek = pool.get(cent, None)
            if not peek:
                peek = pool[cent] = EntityView(cent)
            ## ALT:HACK: clone rect size from old.preview
            ##   FAIL: on startup there is no "old.preview"
            ##     BUT: you can't create one in "__init__" either
            ##       COS you need to wait until .resize() to calc abs-sz
            #   peek._wdg.resize(*self._preview._wdg.sizehw)
            # BAD:PERF: we calc() preview= coords AGAIN here (after _resize_layout)
            wdg = peek._wdg
            wdg.resize(vh, w, origin=(vy, vx))
            vx += w
        # return pool.get(self.focused_ent)

    def _resize_layout(self, vh: int, vw: int, vy: int, vx: int) -> None:
        # pylint:disable=protected-access
        wprevloci = self._layout["prevloci"]
        for i, w in enumerate(wprevloci, start=-len(wprevloci)):
            if prev := self._hist.get_relative(i):
                prev._wdg.resize(vh, w, origin=(vy, vx))
                # RND: we make leftmost column navigatable when we "collapse" to showing only RootNode
                #   CHECK:WF: is it intuitive or irritating ?
                vx += w

        for w in self._layout["browse"]:
            ## ALT:(origin): do C.move(y,x) b4 .redraw(), and remember getyx() inside each .redraw()
            self._view._wdg.resize(vh, w, origin=(vy, vx))
            vx += w

        # CHG: use external shared `CachedEntities inof `HistoryCursor
        wdg = self._hist.focused_view._wdg
        for w in self._layout["preview"]:
            if not wdg._lst:
                break
            peek = self._hist._cache_pool.get(wdg.focused_item._ent)
            if not peek:
                break  # COS: consequent previews are depending on previous ones
            wdg = peek._wdg
            wdg.resize(vh, w, origin=(vy, vx))
            vx += w

    def redraw(self, stdscr: C.window) -> tuple[int, int]:
        wprevloci, wbrowse, wpreview = (self._layout[nm] for nm in self._cfg_layout)
        # pylint:disable=protected-access
        wprevloci = self._layout["prevloci"]
        for i, w in enumerate(wprevloci, start=-len(wprevloci)):
            if prev := self._hist.get_relative(i):
                prev._wdg.redraw(stdscr, numcol=False)

        # CHG: use external shared `CachedEntities inof `HistoryCursor
        # NOTE: directly draw "preview" panel/entity from _pool
        #   &why to avoid constantly rewriting history on each cursor move up/down
        wdg = self._hist.focused_view._wdg
        for w in self._layout["preview"]:
            if not wdg._lst:
                break
            peek = self._hist._cache_pool.get(wdg.focused_item._ent)
            if not peek:
                break  # COS: consequent previews are depending on previous ones
            wdg = peek._wdg
            wdg.redraw(stdscr, numcol=False)

        # NOTE: draw main Browse column very last to always be on top
        curyx = (0, 0)
        for w in self._layout["browse"]:
            curyx = self._view._wdg.redraw(stdscr, numcol=True)
        # TODO: return curyx from focused panel inof the last one on the right
        return curyx
