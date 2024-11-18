import _curses as C

from ..curses_ext import g_style as S
from .entity_base import Representable
from .entries import ErrorEntry
from .navi import NaviWidget


class RootWidget:
    _wh: int
    _ww: int

    # USE? {RootWidget(HaltEntry("NOTHING"))} for a "default" ctor
    def __init__(self, ent: Representable) -> None:
        self.set_entity(ent)

    def set_entity(self, ent: Representable) -> None:
        # FUT: may create different widgets based on `Entity and `Policy
        self._navi = NaviWidget(ent)

    ## DISABLED: we need explicit methods for type-checking
    ##   and to appropriately update header/footer on action
    # def action[**P](self, name: str, *args: P.args, **kwargs: P.kwargs) -> None:
    #     getattr(self, name)(*args, **kwargs)

    # def cursor_jump_to(self, idx: int) -> None:
    #     # pylint:disable=protected-access
    #     self._navi.cursor_jump_to(idx)
    #
    # def cursor_step_by(self, steps: int) -> None:
    #     # pylint:disable=protected-access
    #     self._navi.cursor_step_by(steps)

    def view_go_into(self) -> None:
        self._navi.view_go_into()
        # self._invalidate_header_footer()

    def view_go_back(self) -> None:
        self._navi.view_go_back()
        # self._invalidate_header_footer()

    def resize(self, stdscr: C.window) -> None:
        self._wh, self._ww = stdscr.getmaxyx()
        orig_yx = (1, 1)
        size_yx = (self._wh - orig_yx[0] - 1, self._ww - orig_yx[1])
        self._navi.resize(*size_yx, origin=orig_yx)

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)
        # FUT: only clear "rest of each line" -- and only if prev line there was longer
        stdscr.clear()
        wdg = self._navi._view._wdg

        # pylint:disable=protected-access
        # ALT:([]): use ⸤⸣ OR ⸢⸥
        header = f"[{self._navi._history_idx+1}⁄{len(self._navi._history_stack)}] "
        stdscr.addstr(0, 0, header, S.auxinfo)
        xpath = wdg.focused_item.loci if wdg._lst else self._navi._view._ent.loci + "/"
        try:
            iname = xpath.rindex("/")
            stdscr.addstr(0, len(header), xpath[:iname], S.footer | C.A_BOLD)
            try:
                ilnum = xpath.index(":", iname)
                stdscr.addstr(0, len(header) + iname, xpath[iname:ilnum], S.item)
                stdscr.addstr(0, len(header) + ilnum, xpath[ilnum:], S.iteminfo)
            except ValueError:
                stdscr.addstr(0, len(header) + iname, xpath[iname:], S.item)
        except ValueError:
            stdscr.addstr(0, len(header), xpath, S.footer | C.A_BOLD)

        self._navi.redraw(stdscr)

        ci = 1 + wdg._cursor_item_lstindex
        sz = len(wdg._lst)
        sortby = "name"
        sortrev = False
        footer = f"--- {ci}/{sz} | by={sortby}{"￪" if sortrev else "￬"}"
        ## DEBUG:NEED:(__main__.py): -X tracemalloc
        # footer += f"  --- {{RAM={__import__("tracemalloc").get_traced_memory()[0]//1024:,}kB}}"
        stdscr.addstr(self._wh - 1, 0, footer, S.footer)

        # NOTE: place real cursor to where list-cursor is, to make tmux overlay selection more intuitive
        cy = wdg._viewport_origin_yx[0]
        if not wdg._lst or isinstance(wdg.focused_item, ErrorEntry):
            cx = 0
        else:
            cx = wdg._viewport_origin_yx[1] + 3  # = len(pfx)
        pos = wdg._viewport_followeditem_linesfromtop
        if 0 <= pos < wdg._viewport_height_lines:
            cy += pos
        stdscr.move(cy, cx)
