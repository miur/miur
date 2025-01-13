import _curses as C

from ..util.logger import log
from .entity_base import Golden
from .navihistory import HistoryCursor
from .view import EntityView


class NaviWidget:
    def __init__(self, hist: HistoryCursor) -> None:
        self._hist = hist
        ## NOTE:(triplet preview): show N previous view on the left and 1 future node on the right
        # LIOR? int=Fixed/Column=-min/max, float=Percent/Ratio=-rest/full
        # self._miller_ratio = (20, 0.4, 0)
        self._miller_ratio = (12, 12, 12, 0.4, 0)
        # self._miller_ratio = (20, 0)
        ## BAD: on narrow window it draws 3rd column over 2nd one, triggering {assert iw>4}
        # self._miller_ratio = (20, 0, 0.5)
        self._view_rect = (0, 0, 0, 0)  # (vh,vw,vy,vx)

    # PERF?IDEA: use @cached and reset by "del self._view" in "view_go_*()"
    # RENAME:(view) make it publicly accessible from keymap:lambda
    @property
    def _view(self) -> EntityView:
        return self._hist.focused_view

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
        vh, vw, vy, vx = self._view_rect
        # return self._hist.focused_view._wdg.resize(vh, vw, origin=(vy, vx))  # <TEMP:DEBUG
        vws = self._calc_abs_width(vw)
        assert vws
        log.info(vws)  # <TEMP:DEBUG
        for i, cw in enumerate(vws, start=2 - len(vws)):
            if view := self._hist.get_relative(i):
                # pylint:disable=protected-access
                ## ALT:(origin): do C.move(y,x) b4 .redraw(), and remember getyx() inside each .redraw()
                view._wdg.resize(vh, cw, origin=(vy, vx))
            vx += cw

    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        self._view_rect = vh, vw, *origin
        self._resize_miller()

    def cursor_jump_to(self, idx: int) -> None:
        # pylint:disable=protected-access
        self._view._wdg.focus_on(idx)

    def cursor_step_by(self, steps: int) -> None:
        # pylint:disable=protected-access
        self._view._wdg.step_by(steps)

    def view_go_into(self) -> None:
        # pylint:disable=protected-access
        pwdg = self._view._wdg
        if not pwdg._lst:
            log.trace("<<EMPTY>>")
            return
        self.view_jump_to(pwdg.focused_item._ent)

    def view_jump_to(self, nent: Golden) -> None:
        self._hist.jump_to(nent)
        # NOTE: resize() *new* wdg to same dimensions as *pwdg*
        self._resize_miller()

    # THINK:SPLIT: `NaviModel which knows when to yeild `RootNode (which holds rootfs/stdin/etc providers)
    def view_go_back(self) -> None:
        self._hist.go_back()
        # NOTE: resize() old/cached wdg, as window may had resized from then.
        self._resize_miller()

    def redraw(self, stdscr: C.window) -> tuple[int, int]:
        # return self._hist.focused_view._wdg.redraw(stdscr, numcol=True)  # <TEMP:DEBUG

        # FIXED: prevent crash when window shrinks past the cursor
        # self.cursor_step_by(0)
        curyx = (0, 0)  # BAD: undeterminable if error
        # DFL:(prio): browser:0 -> preview:1 -> parent:2 [-> pparent:3]
        # [_] FIXME:BET: directly draw "preview" panel/entity from _pool
        #   &why to avoid constantly rewriting history on each cursor move up/down
        for i in range(2 - len(self._miller_ratio), 2):
            if view := self._hist.get_relative(i):
                # pylint:disable=protected-access
                if i == 0:
                    curyx = view._wdg.redraw(stdscr, numcol=True)
                else:
                    view._wdg.redraw(stdscr, numcol=False)
        return curyx

    # USE: log.info(str(wdg))
    # def __str__(self) -> str:
    #     s = "  " * 0 + str(0) + ": " + self._ent.name
    #     for i, x in enumerate(self._lstpxy, start=1):
    #         s += "\n  " * 1 + str(i) + ": " + x.name
    #     s += "\r"
    #     s += str(v) if isinstance((v := self._valpxy.get()), int) else repr(v)
    #     return s
