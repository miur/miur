import os.path as fs

import _curses as C

from ..util.logger import log
from .entity_base import Golden, Representable
from .entries import ErrorEntry, FSEntry
from .view import EntityView


# ENH:ADD: triplet preview (Miller)
class NaviWidget:
    _view: EntityView

    def __init__(self, ent: Golden) -> None:
        # NOTE: we create a separate `SatelliteViewport per each `Entity assigned
        #   NICE: preserve "pos,vh,margin" as-is, and then reinterpret/resize only when going back
        #   ++ NICE: preserve the *generated* items as-is in the "_wdg._lst"
        #   +++ NICE: can use totally different *widgets* based on the type(_ent)
        #     e.g. Dashboard or Editor
        self._view = EntityView(ent)
        # MAYBE:CHG: directly store "ent" in _stack to represent "xpath",
        #   as now we can use "_pool" -- to map it to temporarily cached "_view"
        #   RENAME? _cursor_chain/navi_stack | _pool_cached_view/_view_pool
        self._history_stack = [self._view]
        self._history_idx = len(self._history_stack) - 1
        # NOTE: tba to restore view when opening previously visited nodes
        self._history_pool = {ent: self._view}
        ## NOTE: show N previous view on the left and 1 future node on the right
        # LIOR? int=Fixed/Column=-min/max, float=Percent/Ratio=-rest/full
        self._miller_ratio = (20, 0, 0.5)
        self._view_rect = (0, 0, 0, 0)  # (vh,vw,vy,vx)

    def _calc_abs_width(self, vw: int) -> list[int]:
        vws = [
            int(w) if w >= 1 else int(w * vw) if w > 0 else 0
            for w in self._miller_ratio
        ]
        flex = vw - sum(vws)
        if flex < 0:
            raise ValueError(self._miller_ratio)
        for i, w in enumerate(self._miller_ratio):
            if w < 0:
                assert -1 < w, "TEMP: Not supported"
                assert vws[i] == 0, "sanity"
                vws[i] = int(-w * flex)
        spacer = (vw - sum(vws)) // sum(1 for w in self._miller_ratio if w == 0)
        for i, w in enumerate(self._miller_ratio):
            if w == 0:
                assert vws[i] == 0, "sanity"
                vws[i] = spacer
        if vw != sum(vws):
            raise ValueError(self._miller_ratio)
        return vws

    def _resize_miller(self) -> None:
        # pylint:disable=protected-access
        vh, vw, vy, vx = self._view_rect
        vws = self._calc_abs_width(vw)
        if not vws:
            raise RuntimeError(vws)
        if len(vws) == 1:
            self._view._wdg.resize(vh, vws[0], origin=(vy, vx))
        elif len(vws) == 2:
            if (pidx := self._history_idx - 1) > 0:
                self._history_stack[pidx]._wdg.resize(vh, vws[0], origin=(vy, vx))
            self._view._wdg.resize(vh, vws[1], origin=(vy, vx + vws[0]))
        else:
            leftmostidx = self._history_idx + 2 - len(vws)
            for i, cw in enumerate(vws):
                if 0 <= (idx := leftmostidx + i) < len(self._history_stack):
                    ## ALT:(origin): do C.move(y,x) b4 .redraw(), and remember getyx() inside each .redraw()
                    self._history_stack[idx]._wdg.resize(vh, cw, origin=(vy, vx))
                vx += cw

    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        self._view_rect = vh, vw, *origin
        self._resize_miller()

    def cursor_jump_to(self, idx: int) -> None:
        # pylint:disable=protected-access
        self._view._wdg.jump_to(idx)

    def cursor_step_by(self, steps: int) -> None:
        # pylint:disable=protected-access
        self._view._wdg.step_by(steps)

    def view_go_into(self) -> None:
        # pylint:disable=protected-access
        pwdg = self._view._wdg
        if not pwdg._lst:
            log.trace("<<EMPTY>>")
            return
        self.view_jump_to(pwdg.focused_item)

    def view_jump_to(self, nent: Representable) -> None:
        if isinstance(nent, ErrorEntry):
            log.trace(nent.name)
            return

        # pwdg = self._view._wdg

        def _histappend() -> None:
            if v := self._history_pool.get(nent):
                self._view = v
            else:
                try:
                    self._view = EntityView(nent)
                except Exception as exc:
                    raise NotImplementedError() from exc
                self._history_pool[nent] = self._view
            self._history_stack.append(self._view)
            self._history_idx = len(self._history_stack) - 1

        # pylint:disable=protected-access
        # NOTE: keep previous navi stack to be able to return to same EntityView
        #   but discard the rest of navi stack if we go into different route
        #   BAD: if we "jump" to unrelated path (e.g. after !ranger),
        #     then going back-n-forth will discard that jumped path from history :(
        if (nidx := self._history_idx + 1) < len(self._history_stack):
            if self._history_stack[nidx]._ent == nent:
                self._history_idx = nidx
                self._view = self._history_stack[nidx]
            else:
                del self._history_stack[nidx:]
                _histappend()
        else:
            _histappend()
        # NOTE: resize() *new* wdg to same dimensions as *pwdg*
        self._resize_miller()

    def view_go_back(self) -> None:
        # pylint:disable=protected-access
        pview = self._view
        if self._history_idx > 0:
            self._history_idx -= 1
            self._view = self._history_stack[self._history_idx]
        elif isinstance(self._view._ent, FSEntry):
            # RND:(controversial): as we basically navigate to *new* nodes to the left,
            #   so we should keep the history of this navigation, but we discard that
            p = self._view._ent._x
            pp = fs.dirname(p)
            if pp != p:
                parent_ent = FSEntry(pp)
                self._view = EntityView(parent_ent)  # , hint_idx=0)
                self._history_stack = [self._view]
                self._history_idx = len(self._history_stack) - 1
                # NOTE: set cursor onto entity you came back from
                for i, e in enumerate(self._view._wdg._lst):
                    if e.name == pview._ent.name:
                        self._view._wdg._viewport_followeditem_lstindex = i
                        self._view._wdg._cursor_item_lstindex = i
                        # BAD: hardcoding pos to avoid last item at top
                        pos = pview._wdg._viewport_height_lines // 2
                        self._view._wdg._viewport_followeditem_linesfromtop = pos
                        break
                if self._view._wdg._cursor_item_lstindex != i:
                    log.error("WTF: pview not found")
        else:
            raise NotImplementedError()
        # NOTE: resize() old/cached wdg, as window may had resized from then.
        self._resize_miller()

    def redraw(self, stdscr: C.window) -> tuple[int, int]:
        # FIXED: prevent crash when window shrinks past the cursor
        # self.cursor_step_by(0)
        curyx = (0, 0)  # BAD: undetectable if error
        leftmostidx = self._history_idx + 2 - len(self._miller_ratio)
        for i in range(len(self._miller_ratio)):
            if 0 <= (idx := leftmostidx + i) < len(self._history_stack):
                if idx == self._history_idx:
                    curyx = self._history_stack[idx]._wdg.redraw(stdscr, numcol=True)
                else:
                    self._history_stack[idx]._wdg.redraw(stdscr, numcol=False)
        return curyx

    # USE: log.info(str(wdg))
    # def __str__(self) -> str:
    #     s = "  " * 0 + str(0) + ": " + self._ent.name
    #     for i, x in enumerate(self._lstpxy, start=1):
    #         s += "\n  " * 1 + str(i) + ": " + x.name
    #     s += "\r"
    #     s += str(v) if isinstance((v := self._valpxy.get()), int) else repr(v)
    #     return s
