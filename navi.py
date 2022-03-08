import curses as C
from typing import Any

from .dom import CursorViewWidget
from .tui import TUI


def navi(**_kw: Any) -> Any:
    return main()


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


def main() -> None:
    wg = CursorViewWidget()
    with TUI() as tui:
        scr = tui.stdscr

        # print(C.COLORS)
        # if C.COLORS < 8:
        #     C.init_pair(1, 7, 0)
        #     C.init_pair(2, 4, 6)
        # else:
        C.init_pair(1, 7, 8)
        C.init_pair(2, 8, 4)

        scr.attron(C.color_pair(1))
        scr.clear()
        scr.refresh()

        ## [_] TRY: stdout/stderr -> normal curses window, instead of fullscreen alt
        ## [_] SEE: how !ranger does this when jumping into shell
        ### WTF: does Alternate screen works or not ?
        # tput = lambda s: tui.otty.write(C.tigetstr(s).decode(tui.otty.encoding))
        # tput("rmcup")
        # print(C.LINES)
        # tput("smcup")

        # C.napms(1500)
        while True:
            draw_all(scr, wg)
            try:
                key = scr.getkey()
            except KeyboardInterrupt:
                break
            # print(key)
            if key in ("q", "d", "\033"):
                break
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
