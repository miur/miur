from enum import IntEnum
from functools import lru_cache
from typing import Final

# from typing_extensions import ReadOnly  # until python=3.15


# IDEA: use indexed colors for Solarized and keep default palette16 untouched
#   FAIL: all other shell utils will use too bright default colors, which is irritating
# FIXME: use actual names from e.g. neomutt, and annotate/reassign with solarized aliases
class Palette(IntEnum):
    black = 0
    red = 1
    gren = 2
    yelw = 3
    blue = 4
    mgnt = 5
    cyan = 6
    white = 7

    darkblack = 8  # bkgr
    orgn = 9
    greydark = content = 10  # content(dim)
    grey = 11  # body light
    greylight = 12  # body dark
    purp = 13
    greyoff = 14  # emphasized
    brightwhite = 15


type Color = int | str | Palette | tuple[int, int, int]


## SEE: /d/airy/color/ctl/000-colors.py
## ALT: full index-tables for wide range of dynamically changing colors (e.g. plasma animation)
#   _FG = (*map(str, range(30, 38)), *map(str, range(90, 98)), *(f'38;5;{i}' for i in range(16, 256)))
#   _BG = (*map(str, range(40, 48)), *map(str, range(100, 108)), *(f'48;5;{i}' for i in range(16, 256)))
#  USAGE: parts = _FG[fg], _BG[bg])
@lru_cache(maxsize=None)
def _color(n: Color, base: int) -> str:
    if isinstance(n, tuple):
        return f"{base + 8};2;{n[0]};{n[1]};{n[2]}"  # Direct 24-bit True Color Printing
    if isinstance(n, str):
        n = Palette[n].value
    if isinstance(n, Palette):
        n = n.value
    return (
        str(base + 9)
        if n < 0
        else str(base + n)
        if n < 8  # noqa:PLR2004
        else str(base + 52 + n)
        if n < 16  # noqa:PLR2004
        else f"{base + 8};5;{n}"
    )


_SGR: Final = {  # <OFF="Select Graphic Rendition"
    # "reset": (0, 0),  # BET? reset=ansicolor() vs dfl=ansicolor(-1,-1)
    "bold": (1, 22),
    "dim": (2, 22),  # RENAME? ="faint"
    "italic": (3, 23),
    "uline": (4, 24),
    "blink": (5, 25),
    "rev": (7, 27),
}


# ex~: "bold-white-on-red": "\033[1;37m;41m"
@lru_cache(maxsize=None)
def ansicolor(
    # *attrs: int,  # USAGE: parts.extend(str(a) for a in attrs)
    fg: Color | None = None,  # ALT:(None):=-2 , but it's error-prone
    bg: Color | None = None,
    **sgr: bool | None,
) -> str:
    # PERF:INFO: directly inserting into bytesarray+decode has no benefits
    #   ATT: "return (aon,aoff)" has no sense, as color can only be reset, but not restored
    parts = [str(_SGR[k][not v]) for k, v in sgr.items() if v is not None]
    if fg is not None:
        parts.append(_color(fg, 30))
    if bg is not None:
        parts.append(_color(bg, 40))
    return f"\033[{';'.join(parts)}m"


RESET: Final = ansicolor()
