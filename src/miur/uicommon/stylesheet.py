from typing import Annotated, Callable, NamedTuple

from .. import log
from .styleids import Aid, StyleId

# BAD: only Pydantic/beartype will verify this constrain
ColorIndex = Annotated[int, range(-2, 256)]

type Effect[T] = T | Callable[[str], T]


# INFO: No __slots__ = () needed for immediate child of NamedTuple (but needed for next inheritance)
class StyleDef(NamedTuple):
    ## FUT: fg/bg="None" meaning allow attrs/color mix-in, but don't touch original unspecified values
    #    OR:(None): use "-2" to ignore (i.e. avoid override)
    # FUT? allow fg/bg=RGBColor(r,g,b)/HSLColor(h,s,l)
    #   ALSO? "... | str" e.g: "red" or "#FF0000"
    # NOTE: use fg/bg=ColorPalette/tuple[ColorIndex] -- to alternate by .effn
    fg: ColorIndex | tuple[ColorIndex, ...] | None = None
    bg: ColorIndex | tuple[ColorIndex, ...] | None = None
    # attrs: str = ""  # ALT:(str): enum.IntFlag -- to allow only possible combo
    bold: bool | None = None
    italic: bool | None = None
    uline: bool | None = None
    # font_family: str = "monospace"
    ## RENAME?(same_as): .base .base_from .copyfrom .use
    #   WHY: to override its value by new fg/bg/attr
    #   IDEA: .mix .mix_chain -- override each prop by latest value
    #   IDEA: .blend -- combine colors into new value
    #     ex~: parametrized colors :: StyleDef(fg=lambda base: base._replace(fg=base.fg+2), same_as="item")
    same_as: str = ""  # : str | Callable[[StyleDef], StyleDef]
    ## effect
    efid: int | str | None = None
    effn: Callable[[str], int] | None = None
    # efctx: = ["timestamp", ...]
    # efargs: object = None
    # efkws: object = None
    efparams: dict[str, object] | None = None


class UnitedStylesheet:
    # DEFAULT = StyleOverride([default=]StyleDef(0x223344, 0x000000), curses=StyleDef(-1, -1))
    # OR:(naming convention): DEFAULT/curses = DEFAULT.override.curses = StyleDef(-1, -1)
    #   >> NICE: this way future-added "DEFAULT_myterm" could use std "DEFAULT_curses" as a base
    default = StyleDef(-1, -1)
    unknown = StyleDef(201, 52, bold=True, italic=True, uline=True)  # toxic-PINK
    item = StyleDef(same_as="default")
    footer = StyleDef(217, 17, bold=True)

    lineidx = StyleDef(0, -1)  # = ansicolor(Palette.black)
    itemidx = StyleDef(10, -1)  # = ansicolor(Palette.greydark)
    itempunct = StyleDef(
        fg=(15, 1, 3, 4),
        # ALT: efid="rainbow", efparams={"rgx": r"[-_.]"})
        effn=lambda t: next((i for i, s in enumerate(".-_", 1) if s in t), 0),
    )

    # MAYBE? should return StyleParams inof StyleDef with all None values substituted
    #   BUT? only .drv knows what actual defaults to use for None?
    @classmethod
    def resolve_by_aid(cls, aid: StyleId) -> StyleDef:
        # NEED: on startup verify bidirectional unique/unmatched keys
        # TBD:OPT: for unknown styles either throw/log, fallback to default, or use toxic-pink
        #   TBD: in __debug__ disable "fallback to default" to catch mismatches
        # TEMP:WKRND: log+fallback as throwing may exit jurigged
        #   ALT:BAD: even if we catch exception in mainloop -- our screen will be half-empty?
        #     ~~ we don't know what's in DisplayList, so we won't know if some elements are missing
        try:
            nm = Aid.get_name(aid).lower()
            c = getattr(cls, nm)
        except Exception as exc:
            log.error(exc)
            c = cls.unknown

        # EXPL: merge recursively-referrent styles
        # WARN: due to jurigged may reload nested styles we should use recursion each time
        #   ALT:IDEA: on Stylesheet.__setattr__ recursively mark sentinels as .invalidate
        #     NEED:TRY? overload __setattr__ to store members hashes/sentinels in some cache
        #       >> in this way .drv can recognize if sentinel had changed to update its cache
        #         ~~ BAD: feels like complexity raises too much
        if bnm := c.same_as:
            # CHG? override only when c.(fg/bg/attr)==-2
            d = list(c)
            while base := getattr(cls, bnm, None):
                for i in range(len(d)):
                    if not d[i]:
                        d[i] = base[i]
                bnm = base.same_as
            c = c._make(d)
        return c
