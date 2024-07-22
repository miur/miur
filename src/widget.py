from typing import Iterable

import _curses as C


def draw_list(stdscr: C.window) -> None:
    i = 0
    hh, _ww = stdscr.getmaxyx()  # C.LINES
    items = self._wg[i : i + hh - 1]
    beg, _end = self._wg._scroll.range(i)
    pos = self._wg.currel
    for i, x in enumerate(items, start=i):
        idx = 1 + beg + i
        stdscr.addstr(i, 0, f"{i + 1:02d}| {idx:03d}:", C.color_pair(2))
        attr = (C.A_REVERSE | C.A_BOLD) if i == pos else C.color_pair(1)
        stdscr.addstr(f" {x}", attr)


def draw_footer(stdscr: C.window) -> None:
    pair = 30
    C.init_pair(pair, 217, 17)
    attr = C.color_pair(pair)
    # info = C.color_content(fg)
    # info = C.COLOR_PAIRS
    #
    # # pylint:disable=protected-access
    # pv = self._wg._scroll._provider
    # idx = 1 + self._wg.curabs
    # sortby = pv._sortby
    # sortrev = "￬" if pv._sortrev else "￪"
    # info = f"{idx}/{len(pv)} | {sortby=}{sortrev}"
    info = 100

    hh, _ww = stdscr.getmaxyx()  # C.LINES
    stdscr.addstr(hh - 1, 0, f"--- {info}", attr)


class FSEntry:
    def __init__(self, path: str) -> None:
        self._x = path

    @property
    def name(self) -> str:
        return repr(self._x)

    def explore(self) -> Iterable["FSEntry"]:
        with __import__("os").scandir(self._x) as it:
            return [FSEntry(e.path) for e in it]


class ListWidget:
    _ent: Representable
    _act: Callable[[], ListCachingProxy[Representable]]
    _lstpxy: ListCachingProxy[Representable]

    def set_entity(self, ent: Representable) -> None:
        self._ent = ent
        self._act = ent.explore
        self._lstpxy = self._act()

    def redraw(self, stdscr: C.window) -> None:
        draw_list(stdscr)
        draw_footer(stdscr)

    # def render(self) -> None:
    #     print("  " * 0 + str(0) + ": " + self._ent.name)
    #     for i, x in enumerate(self._lstpxy, start=1):
    #         print("  " * 1 + str(i) + ": " + x.name)
    #     s = str(v) if isinstance((v := self._valpxy.get()), int) else repr(v)
    #     print("\r" + s)
