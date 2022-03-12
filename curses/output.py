import asyncio
import curses as C

from ..widgets.scrolllist import CursorViewWidget
from .device import CursesDevice


class CursesOutput:
    def __init__(self, iodev: CursesDevice, wg: CursorViewWidget):
        self._scr = iodev.scr
        self._wg = wg
        self._ev_screen_refresh = asyncio.Event()

    def invalidate(self) -> None:
        self._ev_screen_refresh.set()

    # FIXME: use passed ww/hh when present
    def resize(self, ww: int = None, hh: int = None) -> None:
        C.update_lines_cols()
        hh, ww = self._scr.getmaxyx()
        # print(hh, ww, C.LINES, C.COLS)
        self._wg.resize(ww, hh - 2)
        self.invalidate()

    def draw_footer(self) -> None:
        fg = 217
        bg = 17
        # info = C.color_content(fg)
        info = C.COLOR_PAIRS
        pair = 30
        C.init_pair(pair, fg, bg)

        scr = self._scr
        hh, _ww = scr.getmaxyx()  # C.LINES
        scr.addstr(hh - 2, 0, "---", C.color_pair(pair))
        scr.addstr(hh - 1, 0, str(info), C.color_pair(pair))

    ## BET?
    # jquast/blessed: Blessed is an easy, practical library for making python terminal apps ⌇⡡⣛⠵⠔
    #   https://github.com/jquast/blessed
    def draw_list(self) -> None:
        i = 0
        hh, _ww = self._scr.getmaxyx()  # C.LINES
        items = self._wg[i : i + hh - 1]
        beg, _ = self._wg._scroll.range(i)
        for i, x in enumerate(items, start=i):
            attr = C.color_pair(2) if i == self._wg.pos else C.color_pair(1)
            self._scr.addstr(i, 0, f"{i:02d}| {beg + i:03d}: {x}", attr)

    def draw_all(self) -> None:
        self._scr.clear()
        self.draw_list()
        self.draw_footer()
        self._scr.refresh()

    # PERF: don't bind "draw" and "handle" in single loop pass
    async def drawloop(self) -> None:
        self.resize()
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
