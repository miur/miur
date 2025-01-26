from types import SimpleNamespace

import _curses as C

# from typing import Iterator, Sequence


# SEE: https://bugs.python.org/issue40284
# CHECK: it seems "S.<Tab>" completion doesn't work
# BAD: not typed at all. BET: generate @dataclass or IntEnum inside function ?
g_style = SimpleNamespace()
_registered_color_pairs: dict[tuple[int, int], int] = {}


def termcolor2(fg: int, bg: int) -> int:
    fgbg = (fg, bg)
    p = _registered_color_pairs.get(fgbg, None)
    if p is None:
        i = len(_registered_color_pairs)
        # CHECK: if newer curses supports more than 256 color pairs
        # REF: https://stackoverflow.com/questions/476878/256-colors-foreground-and-background
        assert i < 256
        C.init_pair(i, fg, bg)
        p = _registered_color_pairs[fgbg] = C.color_pair(i)
    return p


def init_colorscheme(stdscr: C.window) -> None:
    # print(C.COLORS)
    # if C.COLORS < 8:
    #     C.init_pair(1, 7, 0)
    #     C.init_pair(2, 4, 6)
    # else:
    C.use_default_colors()

    S = g_style
    S.hardcoded = termcolor2(C.COLOR_WHITE, C.COLOR_BLACK)  # C.color_pair(0)

    S.default = termcolor2(-1, -1)  # DFL: gray text on transparent bkgr
    S.item = S.default
    S.itemalt = termcolor2(-1, 234)  # USAGE: combine with item's dynamic fg=
    S.auxinfo = termcolor2(10, -1)
    S.iteminfo = termcolor2(0, -1)
    S.pfxrel = S.auxinfo
    S.pfxidx = S.iteminfo
    S.cursor = C.A_REVERSE | C.A_BOLD  # OR: termcolor2(8, 4)
    S.cursoralt = S.cursor | C.A_DIM  # FAIL: DIM is ignored when together with REVERSE
    S.mark = termcolor2(61, -1)  # 13/purple
    S.footer = termcolor2(217, 17)
    S.error = termcolor2(160, -1)  # 1
    S.empty = S.error
    S.fsdir = termcolor2(33, -1)  # 4
    S.fslink = termcolor2(37, -1)  # 6
    S.fsexe = termcolor2(64, -1)  # 2

    # pvis = C.curs_set(visibility=0)
    stdscr.attron(S.default)
