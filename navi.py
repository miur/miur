import asyncio
import curses as C
from typing import Any

from .dom import CursorViewWidget
from .tui import TUI


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
    items = wg[i : i + C.LINES - 1]
    for i, x in enumerate(items, start=i):
        attr = C.color_pair(2) if i == wg.pos else C.color_pair(1)
        scr.addstr(i, 0, f"{i}: {x}", attr)


def draw_all(scr: C.window, wg: CursorViewWidget) -> None:
    scr.clear()
    draw_list(scr, wg)
    draw_footer(scr)
    scr.refresh()


async def run(tui: TUI, wg: CursorViewWidget) -> None:
    scr = tui.scr
    scr.nodelay(True)  # non-blocking .getch()
    while True:
        # PERF: don't bind "draw" and "handle" in single loop pass
        draw_all(scr, wg)

        # HACK:(getch-ungetch-getkey): avoid throwing exception
        ch = scr.getch()
        if ch == C.ERR:
            # BET: wait on STDIN
            #   asyncio.get_event_loop().add_reader(fd=0, callback=lambda: on_xcb_ready(xconn, fsm))
            await asyncio.sleep(0.02)
            continue

        C.ungetch(ch)
        key = scr.get_wch()
        # key = scr.getkey()

        if key in ("q", "d", "\033"):
            break
        # if key == C.KEY_RESIZE:
        #     draw_all(scr, wg)
        print(repr(key), file=__import__("sys").stderr)
        handle_keys(key, wg)


def handle_keys(key: str, wg: CursorViewWidget) -> None:
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
        # pvis = curs_set(visibility=0)
        try:
            coro = run(tui, wg)
            asyncio.run(coro)
        except KeyboardInterrupt:
            pass


#%% NEED %gui asyncio
def _live() -> None:
    wg = CursorViewWidget()
    ctx = TUI()
    tui = ctx.__enter__()
    # await run(tui, wg)
    fut = asyncio.create_task(run(tui, wg))

    def _quit() -> None:
        asyncio.all_tasks()
        fut.cancel()
        # BAD: should exit manually after 'q' stops loop
        ctx.__exit__(None, None, None)
