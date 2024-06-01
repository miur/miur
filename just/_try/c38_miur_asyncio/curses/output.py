import asyncio
import curses as C

from ..widgets.scrolllist import CursorViewWidget
from .device import CursesDevice

# DEBUG
#  export NCURSES_TRACE=9


class CursesOutput:
    def __init__(self, iodev: CursesDevice, wg: CursorViewWidget):
        self._scr = iodev.scr
        self._wg = wg
        self._ev_screen_refresh = asyncio.Event()

    def invalidate(self) -> None:
        self._ev_screen_refresh.set()

    def resize(self, w: int = None, h: int = None) -> None:
        if w is None or h is None:
            # C.update_lines_cols()
            # hh, ww = C.COLS, C.LINES
            hh, ww = self._scr.getmaxyx()
            if w is None:
                w = ww
            if h is None:
                h = hh
        else:
            # C.resizeterm(h, w)
            self._scr.resize(h, w)
        # self._scr.clear()
        # self._wg.resize(w, h - 1)  # HACK: keep space for footer
        self._wg.resize(w, (h - 1) // 2)  # HACK: multi-line output
        self.invalidate()
        # self._scr.refresh()

    def draw_footer(self) -> None:
        pair = 30
        C.init_pair(pair, 217, 17)
        attr = C.color_pair(pair)
        # info = C.color_content(fg)
        # info = C.COLOR_PAIRS

        # pylint:disable=protected-access
        pv = self._wg._scroll._provider
        idx = 1 + self._wg.curabs
        sortby = pv._sortby
        sortrev = "￬" if pv._sortrev else "￪"
        info = f"{idx}/{len(pv)} | {sortby=}{sortrev}"

        scr = self._scr
        hh, _ww = scr.getmaxyx()  # C.LINES
        scr.addstr(hh - 1, 0, f"--- {info}", attr)

    ## BET?
    # jquast/blessed: Blessed is an easy, practical library for making python terminal apps ⌇⡡⣛⠵⠔
    #   https://github.com/jquast/blessed
    def draw_list(self) -> None:
        i = 0
        hh, _ww = self._scr.getmaxyx()
        # BAD: unable to print "part" of last item
        items = self._wg[i : i + ((hh - 1) // 2)]
        beg, _end = self._wg._scroll.range(i)
        pos = self._wg.currel
        for i, x in enumerate(items, start=i):
            idx = 1 + beg + i
            self._scr.addstr(i * 2, 0, f"{i + 1:02d}| {idx:03d}:", C.color_pair(2))
            attr = (C.A_REVERSE | C.A_BOLD) if i == pos else C.color_pair(1)
            self._scr.addstr(f" {x}", attr)
            self._scr.addstr(
                i * 2 + 1, 8, f"{x.strsize()} | {x.strdepsnum()}", C.color_pair(3)
            )

    def draw_list1(self) -> None:
        i = 0
        hh, _ww = self._scr.getmaxyx()  # C.LINES
        items = self._wg[i : i + hh - 1]
        beg, _end = self._wg._scroll.range(i)
        pos = self._wg.currel
        for i, x in enumerate(items, start=i):
            idx = 1 + beg + i
            self._scr.addstr(i, 0, f"{i + 1:02d}| {idx:03d}:", C.color_pair(2))
            attr = (C.A_REVERSE | C.A_BOLD) if i == pos else C.color_pair(1)
            self._scr.addstr(f" {x}", attr)

    def draw_all(self) -> None:
        self._scr.clear()

        # BAD: "way to draw" should be part of PacmanItem itself
        # ALSO: mixed list should draw each item differently (or force unified view)
        if self._wg[0].__class__.__name__ == 'PacmanItem':
            self.draw_list()
        else:
            self.draw_list1()

        self.draw_footer()
        # ALSO:(force-full-redraw): self._scr.touchwin()
        #   BET: redraw = lambda: wrefresh(curscr)
        self._scr.refresh()

    # PERF: don't bind "draw" and "handle" in single loop pass
    async def drawloop(self) -> None:
        # self.resize()
        # HACK: reduce CPU consumption in laptop powersave mode
        dtframe = 1 / 15  # 60
        monotime = asyncio.get_running_loop().time
        try:
            while True:
                ts = monotime()
                self.draw_all()
                self._ev_screen_refresh.clear()
                # IDEA: use .invalidate() to mark region for redraw and semaphor to wait in loop until it's triggered
                #   HACK: to prevent too frequent polling/redraw -- measure "dtrun" and "dtwait"
                #   and sleep till the end of Vsync frame before applying accumulated changes
                await self._ev_screen_refresh.wait()
                dt = monotime() - ts
                if dt < dtframe:
                    await asyncio.sleep(dtframe - dt)

        except asyncio.CancelledError:
            pass
