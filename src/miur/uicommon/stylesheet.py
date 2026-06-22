from typing import Annotated, NamedTuple

from .styleids import Aid, StyleId

# BAD: only Pydantic/beartype will verify this constrain
ColorIndex = Annotated[int, range(-2, 256)]


# INFO: No __slots__ = () needed for immediate child of NamedTuple (but needed for next inheritance)
class StyleDef(NamedTuple):
    ## FUT: fg/bg="None" meaning allow attrs/color mix-in, but don't touch original unspecified values
    # FUT? RGBColor(r,g,b) or HSLColor(h,s,l)
    # ALSO: allow fg:ColorPalette
    fg: ColorIndex | None = None  # OR:(None): use "-2" to ignore (i.e. avoid override)
    bg: ColorIndex | None = None  # "... | str" e.g., "red" or "0xFF0000"
    # attrs: str = ""  # ALT:(str): enum.IntFlag -- to allow only possible combo
    bold: bool = False
    italic: bool = False
    # font_family: str = "monospace"
    ## RENAME?(same_as): .base .base_from .copyfrom .use
    #   WHY: to override its value by new fg/bg/attr
    #   IDEA: .mix .mix_chain -- override each prop by latest value
    #   IDEA: .blend -- combine colors into new value
    #     ex~: parametrized colors :: StyleDef(fg=lambda base: base._replace(fg=base.fg+2), same_as="item")
    same_as: str = ""  # : str | Callable[[StyleDef], StyleDef]
    ## effect
    efid: int | str | None = None
    # effn: object = 0
    # efctx: = ["timestamp", ...]
    # efargs: object = None
    # efkws: object = None
    efparams: dict[str, object] | None = None


class UnitedStylesheet:
    # DEFAULT = StyleOverride([default=]StyleDef(0x223344, 0x000000), curses=StyleDef(-1, -1))
    # OR:(naming convention): DEFAULT/curses = DEFAULT.override.curses = StyleDef(-1, -1)
    #   >> NICE: this way future-added "DEFAULT_myterm" could use std "DEFAULT_curses" as a base
    default = StyleDef(-1, -1)
    item = StyleDef(same_as="default")
    footer = StyleDef(217, 17, bold=True)

    # ALT:IDEA: use fg/bg: ColorPalette/tuple[ColorIndex] -- to alternate them based on rgx
    # ITEMPUNCT = StyleDef(efid="rainbow", efparams={"rgx": r"[-_.]"})
    lineidx = StyleDef(0, -1)  # = ansicolor(Palette.black)
    itemidx = StyleDef(10, -1)  # = ansicolor(Palette.greydark)

    # MAYBE? should return StyleParams inof StyleDef with all None values substituted
    #   BUT? only .drv knows what actual defaults to use for None?
    @classmethod
    def resolve_by_aid(cls, aid: StyleId) -> StyleDef:
        nm = Aid.get_name(aid).lower()
        # TBD:OPT: for unknown styles either throw/log, fallback to default, or use toxic-pink
        #   TBD: in __debug__ disable "fallback to default" to catch mismatches
        # NEED: on startup verify none has unique/unmatched keys
        c = getattr(cls, nm)

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
