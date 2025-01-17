import _curses as C

from ..curses_ext import g_style as S

# from ..util.logger import log
from .entity_base import Golden
from .navi import NaviWidget
from .navihistory import HistoryCursor


class RootWidget:
    _wh: int
    _ww: int

    # USE? {RootWidget(HaltEntry("NOTHING"))} for a "default" ctor
    def __init__(self, ent: Golden) -> None:
        self.set_entity(ent)

    def set_entity(self, ent: Golden) -> None:
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

    def resize(self, wh: int, ww: int) -> None:
        self._wh, self._ww = wh, ww
        orig_yx = (1, 1)
        size_yx = (self._wh - orig_yx[0] - 1, self._ww - orig_yx[1])
        ## ALT:(origin): do C.move(y,x) b4 .redraw(), and remember getyx() inside each .redraw()
        self._navi.resize(*size_yx, origin=orig_yx)

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)
        # FUT: only clear "rest of each line" -- and only if prev line there was longer
        stdscr.clear()
        wdg = self._navi._view._wdg

        # pylint:disable=protected-access
        # ALT:([]): use ⸤⸣ OR ⸢⸥
        hidx, hlen = self._navi._hist.pos
        header = f"[{hidx}/{hlen-1}] "  # NOTE:FMT: "0/0" means we are at RootNode with no history
        stdscr.addstr(0, 0, header, S.auxinfo)
        xpath = (
            wdg.focused_item._ent.loci if wdg._lst else self._navi._view._ent.loci + "/"
        )

        # FUT: "compress" header beside simply "cutting" it
        def capx() -> int:
            return stdscr.getmaxyx()[1] - stdscr.getyx()[1]

        try:
            iname = xpath.rindex("/")
        except ValueError:
            stdscr.addnstr(xpath, capx(), S.footer | C.A_BOLD)
        else:
            if (lim := capx()) > 0:
                stdscr.addnstr(xpath[:iname], lim, S.footer | C.A_BOLD)
                if (lim := capx()) > 0:
                    try:
                        ilnum = xpath.index(":", iname)
                    except ValueError:
                        stdscr.addnstr(xpath[iname:], lim, S.item)
                    else:
                        stdscr.addnstr(xpath[iname:ilnum], lim, S.item)
                        if (lim := capx()) > 0:
                            stdscr.addnstr(xpath[ilnum:], lim, S.iteminfo)

        # CHECK: is it still needed ?
        # FIXED: prevent crash when window shrinks past the cursor
        # self._navi.cursor_step_by(0)
        cy, cx = self._navi.redraw(stdscr)

        ci = 1 + wdg._cursor_item_lstindex
        # NICE:IDEA: make scrollbar ~imprecise/elastic~ and scroll through *items* when jumping far,
        #   but scroll by lines when scrolling around current viewport, where we already know all items heights
        sz = len(wdg._lst)
        sortby = "name"
        sortrev = False
        footer = f"--- {ci:2d}/{sz} | by={sortby}{"￪" if sortrev else "￬"}"
        ## DEBUG:NEED:(__main__.py): -X tracemalloc
        # footer += f"  --- {{RAM={__import__("tracemalloc").get_traced_memory()[0]//1024:,}kB}}"

        from ..app import g_app

        modal = "[" + g_app.keytablename + "]"

        # HACK:PERF: skip the check if you haven't even loaded jupyter plugin into !miur
        if __package__.partition(".")[0] + ".util.jupyter" in __import__("sys").modules:
            from ..util.jupyter import g_running_ipykernel

            pfx = "+" if g_running_ipykernel else "-"
            modal = f"({pfx}jupyter) {modal}"

        if (spacerlen := self._ww - len(footer) - len(modal) - 1) > 0:
            footer += " " * spacerlen + modal
        stdscr.addnstr(self._wh - 1, 0, footer, self._ww, S.footer)

        # NOTE: place real cursor to where list-cursor is, to make tmux overlay selection more intuitive
        stdscr.move(cy, cx)
