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

    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        # pylint:disable=protected-access
        self._view._wdg.resize(vh, vw, origin=origin)

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

        pwdg = self._view._wdg

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
        self._view._wdg.resize(
            pwdg._viewport_height_lines,
            pwdg._viewport_width_columns,
            origin=pwdg._viewport_origin_yx,
        )

    def view_go_back(self) -> None:
        # pylint:disable=protected-access
        pwdg = self._view._wdg
        if self._history_idx > 0:
            self._history_idx -= 1
            self._view = self._history_stack[self._history_idx]
        elif isinstance(self._view._ent, FSEntry):
            # RND:(controversial): as we basically navigate to *new* nodes to the left,
            #   so we should keep the history of this navigation, but we discard that
            parent_ent = FSEntry(fs.dirname(self._view._ent._x))
            self._view = EntityView(parent_ent)  # , hint_idx=0)
            self._history_stack = [self._view]
            self._history_idx = len(self._history_stack) - 1
        else:
            raise NotImplementedError()
        # NOTE: resize() old/cached wdg, as window may had resized from then.
        self._view._wdg.resize(
            pwdg._viewport_height_lines,
            pwdg._viewport_width_columns,
            origin=pwdg._viewport_origin_yx,
        )

    def redraw(self, stdscr: C.window) -> None:
        # FIXED: prevent crash when window shrinks past the cursor
        # self.cursor_step_by(0)
        # NOTE: actually _lst here stands for a generic _augdbpxy with read.API
        #   i.e. DB augmented by virtual entries, all generated-and-cleared on demand
        self._view._wdg.redraw(stdscr)

    # USE: log.info(str(wdg))
    # def __str__(self) -> str:
    #     s = "  " * 0 + str(0) + ": " + self._ent.name
    #     for i, x in enumerate(self._lstpxy, start=1):
    #         s += "\n  " * 1 + str(i) + ": " + x.name
    #     s += "\r"
    #     s += str(v) if isinstance((v := self._valpxy.get()), int) else repr(v)
    #     return s
