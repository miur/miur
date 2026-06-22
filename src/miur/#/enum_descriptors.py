import curses as C
from typing import ClassVar, Protocol, Self, cast, overload


class HasContext(Protocol):
    stdscr: C.window
    registered_color_pairs: dict[int, int]
    chr2attr: dict[str, int]


# TEMP: we are converting dynamic .aid solely to static fg/bg/attr cursesstyle
class StyleDef[T: HasContext]:  # RENAME? LazyStyle
    """A descriptor that self-destructs on first access, replacing itself with a raw int."""

    def __init__(
        self,
        fg: int = -1,
        bg: int = -1,
        attrs: str = "",
        same_as: str = "",
    ) -> None:
        self.fg = fg
        self.bg = bg
        self.attrs = attrs
        self.same_as = same_as
        self.name: str

    def __set_name__(self, owner: type[T], name: str) -> None:
        """Automatically captures the property variable name on assignment"""
        self.name = name

    @overload  # 1. Type hint for Class-level access: CursesStyle.HEADER -> returns StyleDef
    def __get__(self, instance: None, owner: type[T]) -> Self: ...
    @overload  # 2. Type hint for Instance-level access: s.HEADER -> returns int
    def __get__(self, instance: T, owner: type[T]) -> int: ...

    def __get__(self, instance: T | None, owner: type[T]) -> int | Self:
        if instance is None:
            return self  # CASE: class-level access (CursesStyle.HEADER)
        if self.same_as:
            # FUT:MAYBE: allow partial fallback i.e. override only when fg/bg/attr=-2
            #   CHECK: if recursive fallback (with destructive caching) works
            curses_attr = getattr(instance, self.same_as)
        else:
            registry = instance.registered_color_pairs
            # EXPL: Internalize using parent context
            fgbg = self.bg * C.COLORS * 2 + self.fg + C.COLORS
            if (curses_attr := registry.get(fgbg, None)) is None:
                i = len(registry)
                assert i < C.COLOR_PAIRS
                C.init_pair(i, self.fg, self.bg)
                curses_attr = registry[fgbg] = C.color_pair(i)
                print((i, fgbg))

            # ALT: next(a for nm, a in [("n": C.A_NORMAL), ...] if nm in self.attrs)
            for a in self.attrs:
                curses_attr |= instance.chr2attr[a]

        # 2. Overwrite descriptor with raw primitive int on the instance
        setattr(instance, self.name, curses_attr)
        print((self.name, curses_attr))
        return cast(int, curses_attr)


# BAD: descriptors are clever, but !jurigged doesn't update them :(
# THINK: no point in overengineering -- we use DisplayList anyway
#   * tui should have IntEnum: name -> styleid(int)
#   * curses should match/convert that IntEnum into internal dict
#      NEED: verify none has unique/unmatched keys
#   * DEV: use "common_basestyle" and auto-generate derived "term_style" with overrides from it
#      WHY: to improve maintainability for colorschemes and switch them at the same time in all UIs
class CursesStyle:
    # DFL:(default): gray text on transparent bkgr
    default: ClassVar[StyleDef[Self]] = StyleDef(-1, -1)
    item: ClassVar[StyleDef[Self]] = StyleDef(same_as="default")
    lineidx: ClassVar[StyleDef[Self]] = StyleDef(0, -1)
    itemidx: ClassVar[StyleDef[Self]] = StyleDef(10, -1)
    footer: ClassVar[StyleDef[Self]] = StyleDef(217, 17, "b")

    def __init__(self, stdscr: C.window) -> None:
        self.stdscr = stdscr
        if not C.has_extended_color_support():
            raise NotImplementedError
        C.start_color()
        C.use_default_colors()
        print(f"{C.COLORS=} {C.COLOR_PAIRS=} {C.termattrs()=:b}")
        assert C.COLORS == (1 << 8), "OBSOL?FIXME: bg<<8 should use inferred mask"

        fgbg = -1 * C.COLORS * 2 - 1 + C.COLORS
        # FIXED: init_pair(-1,-1)==0 always exist (and reset by .use_default_colors())
        self.registered_color_pairs: dict[int, int] = {fgbg: 0}

        # CHG: use enum.IntFlag16
        self.chr2attr = {
            "": 0,  # do nothing
            "n": C.A_NORMAL,  # Normal display (no highlight)
            "r": C.A_REVERSE,  # Reverse video
            "b": C.A_BOLD,  # Extra bright or bold
            "i": C.A_ITALIC,  # Italics (non-X/Open extension)
            "u": C.A_UNDERLINE,  # Underlining
            "s": C.A_STANDOUT,  # Best highlighting mode available
            "d": C.A_DIM,  # Half bright
            "k": C.A_BLINK,  # Blinking
            "v": C.A_INVIS,  # Invisible or blank mode
        }

## USAGE:
# cursesstyle = CursesStyle(stdscr=self.stdscr)
# attr = getattr(self.cursesstyle, Aid.get_name(aid).lower())
# self.stdscr.addnstr(y, x, text, wc, attr)
