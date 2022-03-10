import asyncio
import curses as C
from time import time
from typing import Any, cast

from just.ext.asyncio import enable_debug_asyncio

from .app import Application
from .dom import CursorViewWidget
from .fragments import handle_keybindings
from .tui import TUI

# BAD: only prints first instance of warning
# from warnings import warn

if __name__ == "__main__":
    navi()


def navi(**_kw: Any) -> Any:
    app = Application()
    app.aws.append(run(app.tui, app.wg))
    try:
        app.run()
    except KeyboardInterrupt:
        pass
    finally:
        app.quit()
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
    beg, _ = wg._scroll.range(i)
    for i, x in enumerate(items, start=i):
        attr = C.color_pair(2) if i == wg.pos else C.color_pair(1)
        scr.addstr(i, 0, f"{i:02d}| {beg + i:03d}: {x}", attr)


def draw_all(scr: C.window, wg: CursorViewWidget) -> None:
    scr.clear()
    draw_list(scr, wg)
    draw_footer(scr)
    scr.refresh()


async def run(tui: TUI, wg: CursorViewWidget) -> None:
    tsk = cast(asyncio.Task, asyncio.current_task())
    loop = asyncio.get_running_loop()
    # RENAME: ev_screen_refresh
    event = asyncio.Event()
    STDIN_FILENO = 0

    # [_] SEIZE: A Python asyncio cancellation pattern | by Rob Blackbourn | Medium ⌇⡢⠨⢣⣉
    #   https://rob-blackbourn.medium.com/a-python-asyncio-cancellation-pattern-a808db861b84
    def fstop() -> None:
        loop.remove_reader(fd=STDIN_FILENO)
        tsk.cancel()

    def resize() -> None:
        C.update_lines_cols()
        hh, ww = scr.getmaxyx()
        # print(hh, ww, C.LINES, C.COLS)
        wg.resize(ww, hh - 2)
        event.set()

    scr = tui.scr
    scr.nodelay(True)  # non-blocking .getch()
    cb = lambda scr=scr, wg=wg: process_input(scr, wg, fstop, event, resize)
    loop.add_reader(fd=STDIN_FILENO, callback=cb)

    try:
        resize()
        # PERF: don't bind "draw" and "handle" in single loop pass

        dtframe = 1 / 60
        while True:
            ts = time()
            draw_all(scr, wg)
            event.clear()
            # IDEA: use .invalidate() to mark region for redraw and semaphor to wait in loop until it's triggered
            #   HACK: to prevent too frequent polling/redraw -- measure "dtrun" and "dtwait"
            #   and sleep till the end of Vsync frame before applying accumulated changes
            await event.wait()
            dt = time() - ts
            if dt < dtframe:
                await asyncio.sleep(dtframe - dt)

    except asyncio.CancelledError:
        pass


def process_input(
    scr: C.window, wg: CursorViewWidget, fstop: Any, event: asyncio.Event, resize: Any
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
            resize()
            continue

        if key in ("q", "\033"):  # "d",
            # print("FIXME: quit loop", file=__import__("sys").stderr)
            fstop()
        handle_keybindings(wg, key)
        event.set()

    if evnum == 0:
        print("WTF: processing woke up w/o input", file=__import__("sys").stderr)


def cancel_all() -> None:
    for t in asyncio.all_tasks():
        tasknm = getattr(t.get_coro(), "__qualname__", None)
        if tasknm != "Kernel.dispatch_queue":
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
