import asyncio
import curses as C
from typing import Any

from just.ext.asyncio import enable_debug_asyncio

from .dom import CursorViewWidget
from .tui import TUI

# BAD: only prints first instance of warning
# from warnings import warn


def navi(**_kw: Any) -> Any:
    main()
    print("clean")


def draw_footer(scr: C.window) -> None:
    fg = 217
    bg = 17
    # info = C.color_content(fg)
    info = C.COLOR_PAIRS
    pair = 30
    C.init_pair(pair, fg, bg)

    scr.addstr(C.LINES - 2, 0, "---", C.color_pair(pair))
    scr.addstr(C.LINES - 1, 0, str(info), C.color_pair(pair))


## BET?
# jquast/blessed: Blessed is an easy, practical library for making python terminal apps ⌇⡡⣛⠵⠔
#   https://github.com/jquast/blessed
def draw_list(scr: C.window, wg: CursorViewWidget) -> None:
    i = 0
    hh, ww = scr.getmaxyx()  # C.LINES
    items = wg[i : i + hh - 1]
    for i, x in enumerate(items, start=i):
        attr = C.color_pair(2) if i == wg.pos else C.color_pair(1)
        scr.addstr(i, 0, f"{i:02d}| {x}", attr)


def draw_all(scr: C.window, wg: CursorViewWidget) -> None:
    scr.clear()
    draw_list(scr, wg)
    draw_footer(scr)
    scr.refresh()


async def run(tui: TUI, wg: CursorViewWidget) -> None:
    scr = tui.scr
    scr.nodelay(True)  # non-blocking .getch()
    cb = lambda scr=scr, wg=wg: process_input(scr, wg)
    STDIN_FILENO = 0
    asyncio.get_running_loop().add_reader(fd=STDIN_FILENO, callback=cb)

    while True:
        # PERF: don't bind "draw" and "handle" in single loop pass
        # IDEA: use .invalidate() to mark region for redraw
        #   and semaphor to wait in loop until it's triggered
        #   HACK: to prevent too frequent polling/redraw -- measure "dtrun" and "dtwait"
        #   and sleep till the end of Vsync frame before applying accumulated changes
        draw_all(scr, wg)
        await asyncio.sleep(0.1)  # TEMP:REM:


def process_input(scr: C.window, wg: CursorViewWidget) -> None:
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
        # if key == C.KEY_RESIZE:
        #     C.update_lines_cols()
        #     hh, ww = scr.getmaxyx()
        #     wg.resize(ww, hh)
        #     continue
        handle_keybindings(key, wg)
    if evnum == 0:
        print("WTF: processing woke up w/o input", file=__import__("sys").stderr)


def handle_keybindings(key: str | int, wg: CursorViewWidget) -> None:
    if key in ("q", "d", "\033"):
        # print("FIXME: quit loop", file=__import__("sys").stderr)

    if key == "j":
        wg.pos += 1
    if key == "k":
        wg.pos -= 1

    # [_] FUTURE: wg.pos = -1
    if key == "g":
        wg.pos = -len(wg)
    if key == "G":
        wg.pos = len(wg)
    if key == "H":
        wg.pos = 0
    if key == "M":
        wg.pos = wg._scroll.height // 2
    if key == "L":
        wg.pos = wg._scroll.height


def main() -> None:
    wg = CursorViewWidget()
    with TUI() as tui:
        try:
            coro = run(tui, wg)
            asyncio.run(coro)
        except KeyboardInterrupt:
            pass


def cancel_all() -> None:
    for t in asyncio.all_tasks():
        if t.get_coro().__qualname__ != "Kernel.dispatch_queue":
            t.cancel()


#%% NEED %gui asyncio
def _live() -> None:
    enable_debug_asyncio()
    wg = CursorViewWidget()
    ctx = TUI()
    tui = ctx.__enter__()
    scr = tui.scr
    fut = asyncio.create_task(run(tui, wg))
    # fut.cancel()
    # await run(tui, wg)

    def _quit() -> None:
        cancel_all()
        asyncio.all_tasks()
        # BAD: should exit manually after 'q' stops loop
        ctx.__exit__(None, None, None)
