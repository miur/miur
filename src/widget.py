import os
from typing import Callable, Iterable, Protocol, Sequence, TypeVar, override

import _curses as C

from .curses_ext import ColorMap
from .util.logger import log


class Representable(Protocol):
    @property
    def name(self) -> str: ...


# ALSO: my earlier View concept is actually a Presenter
#   : VisibleContext=ScrollWindow+Cursor+Selections+OverlayHints+Tags+etc.
class VisibleContext:
    wdgh: int
    wdgw: int
    wndabsoff0: int
    wndmaxlen: int
    wndcurpos0: int


def draw_list(
    stdscr: C.window, lst: Sequence[Representable], vctx: VisibleContext
) -> None:
    log.verbose(f"list: [<={vctx.wndmaxlen}/{len(lst)}]")

    def _pfx(i: int) -> str:
        idx = 1 + i + vctx.wndabsoff0
        cur = ">" if i == vctx.wndcurpos0 else ":"
        return f"{1+i:02d}| {idx:03d}{cur} "

    citem = C.color_pair(ColorMap.default)
    caux = C.color_pair(ColorMap.auxinfo)
    ccurs = C.A_REVERSE | C.A_BOLD  # OR: C.color_pair(ColorMap.cursor)

    def _draw_item_at(i: int, attr: int) -> None:
        idx = i + vctx.wndabsoff0
        pfx = _pfx(i)
        stdscr.addstr(i, 0, pfx, caux)
        text = lst[idx].name
        stdscr.addstr(text, attr)
        log.verbose(f"{pfx}{text}")  # :{attr}:

    for i in range(0, min([vctx.wndmaxlen, len(lst)])):
        _draw_item_at(i, citem)

    ## NOTE: draw cursor AGAIN after footer (i.e. over-draw on top of full list)
    ##   NICE: no need to hassle with storing cursor prefix length for cx/cy
    ##   NICE: can redraw only two lines (prev item and cursor) inof whole list
    # cx = len(_pfx(vctx.wndcurpos0))
    # _draw_item_at(vctx.wndcurpos0, citem)
    # stdscr.move(vctx.wndcurpos0, cx)
    ## OR:BET: only change attributes of already printed line
    cx = len(_pfx(vctx.wndcurpos0))
    cn = len(lst[vctx.wndcurpos0 + vctx.wndabsoff0].name)
    stdscr.chgat(vctx.wndcurpos0, cx, cn, ccurs)


# def draw_footer(stdscr: C.window) -> None:
#     pair = 30
#     C.init_pair(pair, 217, 17)
#     attr = C.color_pair(pair)
#     # info = C.color_content(fg)
#     # info = C.COLOR_PAIRS
#     #
#     # # pylint:disable=protected-access
#     # pv = self._wg._scroll._provider
#     # idx = 1 + self._wg.curabs
#     # sortby = pv._sortby
#     # sortrev = "￬" if pv._sortrev else "￪"
#     # info = f"{idx}/{len(pv)} | {sortby=}{sortrev}"
#     info = 100
#
#     hh, _ww = stdscr.getmaxyx()  # C.LINES
#     stdscr.addstr(hh - 1, 0, f"--- {info}", attr)


class FSEntry(Representable):
    def __init__(self, path: str) -> None:
        self._x = path

    @override
    @property
    def name(self) -> str:
        return repr(self._x)

    # i.e. =InterpretUnchangedDirListingPropertyAsFSEntriesInUsualWay
    def explore(self) -> Iterable["FSEntry"]:
        with os.scandir(self._x) as it:
            return [FSEntry(e.path) for e in it]


# T = TypeVar("T")
# class ListCachingProxy(list[T]):
#     pass


class RootWidget:
    _ent: Representable
    _lst: Sequence[Representable]
    # _lstpxy: ListCachingProxy[Representable]
    _act: Callable[[], Sequence[Representable]]
    _vctx: VisibleContext

    def set_entity(self, ent: Representable) -> None:
        c = VisibleContext()
        c.wndmaxlen = 0  # WKRND:BAD: we don't have stdscr here to getmaxyx()
        c.wndabsoff0 = 0
        c.wndcurpos0 = 0  # MAYBE: keep prev cursor position if ent refers to same loci
        self._vctx = c
        self._ent = ent
        if (sfn := getattr(ent, "explore")) and callable(sfn):
            self._act = sfn  # NOTE: keep sfn to be able to refresh() the list (when externally changed)
            self._lst = self._act()

    def cursor_move_rel(self, modifier: int) -> None:
        c = self._vctx
        newpos = c.wndcurpos0 + modifier
        c.wndcurpos0 = max(min(newpos, c.wndmaxlen - 1, len(self._lst) - 1), 0)

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)

        c = self._vctx
        c.wdgh, c.wdgw = stdscr.getmaxyx()
        log.info(f"draw: [{c.wdgh}x{c.wdgw}]")
        # FIXME: "-1" should be externally calculated by `Layout, based on "Footer.height"
        c.wndmaxlen = max(c.wdgh - 1, 0)
        # wndabsoff0: beg, _end = self._wg._scroll.range(i)
        # wndcurpos0: pos = self._wg.currel

        # NOTE: actually _lst here stands for a generic _augdbpxy with read.API
        #   i.e. DB augmented by virtual entries, all generated-and-cleared on demand
        draw_list(stdscr, self._lst, c)
        # draw_footer(stdscr)

    # USE: log.info(str(wdg))
    # def __str__(self) -> str:
    #     s = "  " * 0 + str(0) + ": " + self._ent.name
    #     for i, x in enumerate(self._lstpxy, start=1):
    #         s += "\n  " * 1 + str(i) + ": " + x.name
    #     s += "\r"
    #     s += str(v) if isinstance((v := self._valpxy.get()), int) else repr(v)
    #     return s


def _live() -> None:
    log.sep()
    from .app import g_app as g
    from .widget import RootWidget

    g.root_wdg = wdg = RootWidget()
    # wdg.set_entity(FSEntry("/etc/udev"))
    wdg.set_entity(FSEntry("/d/airy"))
    wdg.redraw(g.stdscr)
    g.stdscr.refresh()
