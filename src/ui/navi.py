from typing import Iterable, Iterator, Self

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

    def __len__(self) -> int:
        return len(self._split)

    def __iter__(self) -> Iterator[Self]:
        return iter(self._split)

    def __getitem__(self, name: str) -> Self:
        # FIXME: search recursively
        return next(p for p in self._split if p.name == name)

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


# TODO:(`AdaptiveLayout): change/hide based on total window size
#   DFL:(prio): browser:0 -> preview:1 -> parent:2 [-> pparent:3]
# TODO: toggle/limit linewrap + content-awareness
# TODO: header/footer hide/xfm
def pick_adaptive_layout_cfg(ww: int, old: Panel) -> Panel:
    if ww < 50:
        raise NotImplementedError(ww)
    if old.name == "navi_main":
        return old
    prevloci = Panel("prevloci", [Panel("pprev", w=0.4), Panel("prev")], w=22)
    # interp = PanelView(ratio=(0,))
    preview = Panel("preview", [Panel("pv", w=0.4), Panel("pvn")])
    # TODO:ALSO: return linewrap/header/footer cfg overrides
    return Panel("navi_main", [prevloci, Panel("browse", w=0.5), preview])


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
        self._layout = pick_adaptive_layout_cfg(vw, self._layout)
        self._layout.resize(Rect(vw, vh, x=orig_yx[1], y=orig_yx[0]))
        log.info(f"{self._layout=}")  # <TEMP:DEBUG

        # BAD:AGAIN: we init very first preview() on very first resize event
        # BAD: we can't discard .resize() from _update_preview, COS we need to do it for older nodes on <j/k>
        #   BUT:NICE: if we could discard it -- we could call _update_preview() from __init__ inof resize(),
        #     and then on startup first ever resize() would put proper sizes for all widgets
        #   SPLIT? make _resize_preview() function to call on <j/k> and achieve everything mentioned ?
        self._update_preview()
        self._resize_cached_widgets()

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
        self._resize_cached_widgets()

    # THINK:SPLIT: `NaviModel which knows when to yeild `RootNode (which holds rootfs/stdin/etc providers)
    def view_go_back(self) -> None:
        self._hist.go_back()
        # WHY: after previous jump_to() we may return to disjoint parent with different preview()
        self._update_preview()
        # NOTE: forced resize() old/cached wdg, as window may had resized from then.
        self._resize_cached_widgets()

    # MAYBE? merge with _resize_cached_widgets(), as they always called together ?
    def _update_preview(self) -> None:
        # pylint:disable=protected-access
        wdg = self._hist.focused_view._wdg
        for p in self._layout["preview"]:
            if not wdg._lst:
                break
            cent = wdg.focused_item._ent
            if isinstance(cent, ErrorEntry):
                break  # TEMP: until I make errors explorable
            peek = self._pool.get(cent)
            if not peek:
                peek = self._pool.add(EntityView(cent))
            ## ALT:HACK: clone rect size from old.preview
            ##   FAIL: on startup there is no "old.preview"
            ##     BUT: you can't create one in "__init__" either
            ##       COS you need to wait until .resize() to calc abs-sz
            #   peek._wdg.resize(*self._preview._wdg.sizehw)
            # BAD:PERF: we calc() preview= coords AGAIN here (after _resize_cached_widgets)
            wdg = peek._wdg
            r = p.rect
            wdg.resize(r.h, r.w, origin=(r.y, r.x))
        # return pool.get(self.focused_ent)

    def _resize_cached_widgets(self) -> None:
        # pylint:disable=protected-access
        # TODO: extend prev.w for full prevloci, when there is single hist node
        #   similarly for 3/5 nodes -- extend 3rd for the space of 4th and 5th
        prevloci = self._layout["prevloci"]
        for i, r in enumerate((p.rect for p in prevloci), start=-len(prevloci)):
            if prev := self._hist.get_relative(i):
                prev._wdg.resize(r.h, r.w, origin=(r.y, r.x))

        # BUG?
        for r in (self._layout["browse"].rect,):
            self._view._wdg.resize(r.h, r.w, origin=(r.y, r.x))

        # CHG: use external shared `CachedEntities inof `HistoryCursor
        wdg = self._hist.focused_view._wdg
        for r in (p.rect for p in self._layout["preview"]):
            if not wdg._lst:
                break
            peek = self._pool.get(wdg.focused_item._ent)
            if not peek:
                break  # COS: consequent previews are depending on previous ones
            wdg = peek._wdg
            wdg.resize(r.h, r.w, origin=(r.y, r.x))

    def redraw(self, stdscr: C.window) -> tuple[int, int]:
        # pylint:disable=protected-access
        wprevloci = self._layout["prevloci"]
        for i, _ in enumerate(wprevloci, start=-len(wprevloci)):
            if prev := self._hist.get_relative(i):
                prev._wdg.redraw(stdscr, numcol=False)

        # CHG: use external shared `CachedEntities inof `HistoryCursor
        # NOTE: directly draw "preview" panel/entity from _pool
        #   &why to avoid constantly rewriting history on each cursor move up/down
        wdg = self._hist.focused_view._wdg
        for _ in self._layout["preview"]:
            if not wdg._lst:
                break
            peek = self._pool.get(wdg.focused_item._ent)
            if not peek:
                break  # COS: consequent previews are depending on previous ones
            wdg = peek._wdg
            wdg.redraw(stdscr, numcol=False)

        # NOTE: draw main Browse column very last to always be on top
        curyx = (0, 0)
        for _ in self._layout["browse"]:
            curyx = self._view._wdg.redraw(stdscr, numcol=True)
        # TODO: return curyx from focused panel inof the last one on the right
        return curyx
