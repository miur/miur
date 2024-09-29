import os
from typing import Callable, Iterable, Protocol, Sequence, TypeVar, override

import _curses as C

from .util.logger import log


class Representable(Protocol):
    @property
    def name(self) -> str: ...


def draw_list(stdscr: C.window, lst: Sequence[Representable]) -> None:
    i = 0
    hh, _ww = stdscr.getmaxyx()  # C.LINES
    items = lst[i : i + hh - 1]
    beg = 0
    pos = 1
    # beg, _end = self._wg._scroll.range(i)
    # pos = self._wg.currel
    for i, x in enumerate(items, start=i):
        idx = 1 + beg + i
        stdscr.addstr(i, 0, f"{i + 1:02d}| {idx:03d}:", C.color_pair(2))
        log.info(f'{i=}')
        text = " " + x.name
        attr = (C.A_REVERSE | C.A_BOLD) if i == pos else C.color_pair(1)
        stdscr.addstr(text, attr)


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

    def set_entity(self, ent: Representable) -> None:
        self._ent = ent
        if (sfn := getattr(ent, "explore")) and callable(sfn):
            self._act = sfn
            self._lst = self._act()

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)
        draw_list(stdscr, self._lst)
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

    g.root_wdg = wdg = RootWidget()
    wdg.set_entity(FSEntry("/etc/udev"))
    wdg.redraw(g.stdscr)
    g.stdscr.refresh()
