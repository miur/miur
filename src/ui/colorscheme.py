from types import SimpleNamespace

import _curses as C

# from typing import Iterator, Sequence


# SEE: https://bugs.python.org/issue40284
# CHECK: it seems "S.<Tab>" completion doesn't work
# BAD: not typed at all. BET: generate @dataclass or IntEnum inside function ?
# TALK: Typing rules for SimpleNamespace - Typing - Discussions on Python.org ⌇⡧⢞⠇⠼
#   https://discuss.python.org/t/typing-rules-for-simplenamespace/60906
# class AttrDict(dict[str, int]):
#     def __init__(self) -> None:
#         super().__init__()
#         self.__dict__ = self
## OR:SRC: http://code.activestate.com/recipes/473786-dictionary-with-attribute-style-access/
# class ReadonlyAttrDict(dict[str, int]):
#   def __getitem__(self, key):
#     value = dict.__getitem__(self, key)
#     return ReadonlyAttrDict(value) if isinstance(value, dict) else value
#   __getattr__ = __getitem__
# g_style = AttrDict()
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
    # pylint:disable=attribute-defined-outside-init
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
    S.mark = termcolor2(61, -1)  # 13=PURP
    S.footer = termcolor2(217, 17)  # 5=PINK
    S.error = termcolor2(160, -1)  # 1=RED
    S.empty = S.error
    S.fsdir = termcolor2(33, -1)  # 4=BLUE
    S.fslink = termcolor2(37, -1)  # 6=CYAN
    S.fsexe = termcolor2(64, -1)  # 2=GREN
    S.fsmnt = termcolor2(63, -1)  # 5=PURP/PINK
    S.action = termcolor2(29, -1)  # 2=GREN

    # pvis = C.curs_set(visibility=0)
    stdscr.attron(S.default)
