import asyncio
import curses as C
from typing import Any, cast, no_type_check

from just.ext.asyncio import enable_debug_asyncio

from .app import Application
from .dom import CursorViewWidget
from .fragments import handle_keybindings
from .tui import TUI

# BAD: only prints first instance of warning
# from warnings import warn

# HACK:(vimspector): allow running this script in debugger
if __name__ == "__main__":
    navi()


def navi(**_kw: Any) -> Any:
    with Application(run) as app:
        app.run()
    print("clean")


class CursesOutput:
    def __init__(self, scr: C.window, wg: CursorViewWidget):
        self._scr = scr
        self._wg = wg
        self._ev_screen_refresh = asyncio.Event()

    def invalidate(self) -> None:
        self._ev_screen_refresh.set()

    def resize(self) -> None:
        C.update_lines_cols()
        hh, ww = self._scr.getmaxyx()
        # print(hh, ww, C.LINES, C.COLS)
        self._wg.resize(ww, hh - 2)
        self.invalidate

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


async def run(tui: TUI, wg: CursorViewWidget) -> None:
    tsk = cast(asyncio.Task, asyncio.current_task())
    loop = asyncio.get_running_loop()
    STDIN_FILENO = 0

    # [_] SEIZE: A Python asyncio cancellation pattern | by Rob Blackbourn | Medium ⌇⡢⠨⢣⣉
    #   https://rob-blackbourn.medium.com/a-python-asyncio-cancellation-pattern-a808db861b84
    def fstop() -> None:
        loop.remove_reader(fd=STDIN_FILENO)
        tsk.cancel()

    scr = tui.scr
    draw = CursesOutput(scr, wg)
    scr.nodelay(True)  # non-blocking .getch()
    cb = lambda scr=scr, wg=wg: process_input(scr, wg, fstop, draw)
    loop.add_reader(fd=STDIN_FILENO, callback=cb)

    await draw.drawloop()


def process_input(
    scr: C.window, wg: CursorViewWidget, fstop: Any, tui: CursesOutput
) -> None:
    # NOTE: only count events, don't accumulate into list for post-processing
    #   << otherwise new events received during processing will become stalled
    #   FUTURE:TRY: read events in bursts (nested while loop) to be able to cancel-out
    #     BAD:PERF: we may constantly linger in this loop, never yielding to asyncio
    #     MAYBE: current single loop already allows "bursts" and giving up CPU time ?
    evnum = 0
    # HACK:(getch-ungetch-getkey): avoid throwing exception
    while (ch := scr.getch()) != C.ERR:
        evnum += 1
        # if ch == C.KEY_RESIZE:
        #     print(ch)
        C.ungetch(ch)
        key = scr.get_wch()
        # key = scr.getkey()
        print(repr(key), file=__import__("sys").stderr)

        ## DEP:configure(--enable-sigwinch)
        # BAD: does not work inside jupyter
        # BAD: should press some key before ev:410 will be reported
        # REGR: redraw() during KEY_RESIZE results in ncurses crash
        #   THINK: how to prevent/block redraw in that case?
        if key == C.KEY_RESIZE:
            tui.resize()
            continue

        if key in ("q", "\033"):  # "d",
            # print("FIXME: quit loop", file=__import__("sys").stderr)
            fstop()
        handle_keybindings(wg, key)
        tui.invalidate()

    if evnum == 0:
        print("WTF: processing woke up w/o input", file=__import__("sys").stderr)


def cancel_all() -> None:
    for t in asyncio.all_tasks():
        tasknm = getattr(t.get_coro(), "__qualname__", None)
        if tasknm != "Kernel.dispatch_queue":
            t.cancel()


#%% NEED %gui asyncio
app: Application


@no_type_check
def _live():
    enable_debug_asyncio()

    global app
    app = Application(run)
    app.__enter__()
    app.attach()
    # TODO app.resize()

    def _debug():
        cancel_all()
        asyncio.all_tasks()

    def _quit(app=app):
        app.shutdown()
        del app
