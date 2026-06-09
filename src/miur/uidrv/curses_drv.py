import curses as C
from typing import ClassVar, Protocol, Self, assert_never, overload

from ..systems.tuisystem import Aid, DisplayList, TextSpan

## Direct 24-bit True Color Printing in Python
# def rgb_text(text, fg_rgb=None, bg_rgb=None):
#     codes = []
#     if fg_rgb:
#         r, g, b = fg_rgb
#         codes.append(f"38;2;{r};{g};{b}")  # Foreground code
#     if bg_rgb:
#         r, g, b = bg_rgb
#         codes.append(f"48;2;{r};{g};{b}")  # Background code
#
#     ansi_sequence = f"\x1b[{';'.join(codes)}m" if codes else ""
#     ansi_reset = "\x1b[0m"
#     return f"{ansi_sequence}{text}{ansi_reset}"
## Print vibrant orange text on a deep navy background
# print(rgb_text("Hello from 24-bit True Color!", fg_rgb=(255, 140, 0), bg_rgb=(10, 25, 47)))


class HasContext(Protocol):
    stdscr: C.window
    registered_color_pairs: dict[int, int]
    chr2attr: dict[str, int]


# TEMP: we are converting dynamic .aid solely to static fg/bg/attr termstyle
class StyleDef[T: HasContext]:  # RENAME? LazyStyle
    """A descriptor that self-destructs on first access, replacing itself with a raw int."""

    def __init__(
        # FUT: fg/bg="None" meaning allow attrs/color mix-in, but don't touch original unspecified values
        self,
        fg: int = -1,
        bg: int = -1,
        attrs: str = "",
        same_as: str = "",
    ) -> None:
        self.fg = fg
        self.bg = bg
        self.attrs = attrs  # ALT:(str): enum.IntFlag -- to allow only possible combo
        ## RENAME?(same_as): .base .base_from .copyfrom .use
        #   WHY: to override its value by new fg/bg/attr
        #   IDEA: .mix .mix_chain -- override each prop by latest value
        #   IDEA: .blend -- combine colors into new value
        self.same_as = same_as
        self.name: str

    def __set_name__(self, owner: type[T], name: str) -> None:
        """Automatically captures the property variable name on assignment"""
        self.name = name

    @overload  # 1. Type hint for Class-level access: TermStyle.HEADER -> returns StyleDef
    def __get__(self, instance: None, owner: type[T]) -> Self: ...
    @overload  # 2. Type hint for Instance-level access: s.HEADER -> returns int
    def __get__(self, instance: T, owner: type[T]) -> int: ...

    def __get__(self, instance: T | None, owner: type[T]) -> int | Self:
        if instance is None:
            return self  # CASE: class-level access (TermStyle.HEADER)
        if self.same_as:
            # FUT:MAYBE: allow partial fallback i.e. override only when fg/bg/attr=-2
            #   CHECK: if recursive fallback (with destructive caching) works
            curses_attr = getattr(instance, self.same_as)
        else:
            registry = instance.registered_color_pairs
            # EXPL: Internalize using parent context
            #   FIXED? positive bg with negative fg goes into prev-bg
            fgbg = self.bg * C.COLORS * 2 + self.fg + C.COLORS
            if (curses_attr := registry.get(fgbg, 0)) == 0:
                i = len(registry)
                # REF: https://stackoverflow.com/questions/476878/256-colors-foreground-and-background
                #   = (65536 if has_extended_color_support() else 256)
                # REF: https://docs.python.org/3/library/curses.html#curses.color_pair
                #   BAD: Only the first 256 color pairs are supported.
                assert i < C.COLOR_PAIRS
                C.init_pair(i, self.fg, self.bg)
                curses_attr = registry[fgbg] = C.color_pair(i)

            # ALT: next(a for nm, a in [("n": C.A_NORMAL), ...] if nm in self.attrs)
            for a in self.attrs:
                curses_attr |= instance.chr2attr[a]

        # 2. Overwrite descriptor with raw primitive int on the instance
        setattr(instance, self.name, curses_attr)
        return curses_attr


# THINK: no point in overengineering -- we use DisplayList anyway
#   * tui should have IntEnum: name -> styleid(int)
#   * curses should match/convert that IntEnum into internal dict
#      NEED: verify none has unique/unmatched keys
#   * DEV: use "common_basestyle" and auto-generate derived "term_style" with overrides from it
#      WHY: to improve maintainability for colorschemes and switch them at the same time in all UIs
class TermStyle:
    # DFL:(default): gray text on transparent bkgr
    default: ClassVar[StyleDef[Self]] = StyleDef(-1, -1)
    item: ClassVar[StyleDef[Self]] = StyleDef(same_as="default")
    footer: ClassVar[StyleDef[Self]] = StyleDef(217, 17)

    def __init__(self, stdscr: C.window) -> None:
        self.stdscr = stdscr
        if not C.has_extended_color_support():
            raise NotImplementedError
        C.start_color()
        C.use_default_colors()
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


# SEP/OPT::
#   * fullscreen (vs embedded piece-of-screen)
#   * curses (vs my own native-tui) (vs Textual/Rich/Asciimatics/Blessed/Urwid/PromptToolkit)
#     - Textual: The most popular, modern async framework for Python terminal layouts.
#         It relies on standard CSS formatting and natively handles 24-bit colors.
#     - Rich: A lighter utility for rich text styling, true-color logging, tables, and terminal formatting.
#     - Asciimatics: Great if you need animations or 24-bit graphics running directly inside your terminal.
#   * TBD: webapp/pygame
class CursesUIDriver:
    def __init__(self) -> None:

        self.stdscr: C.window
        self._pvis: int
        self.termstyle: TermStyle

    def __enter__(self) -> Self:
        C.setupterm()
        self.stdscr = C.initscr()
        C.noecho()
        C.raw()
        self.stdscr.keypad(True)
        self._pvis = C.curs_set(0)
        ## DISABLED: currently I use blocking while-loop
        ##   ALT: py$ try: get_wch() ; except curses.error: pass; curses.napms(100)
        # self.stdscr.nodelay(True)
        ## BAD: maybe keys should be assigned only *after* initscr
        ##   BUT:FAIL? can't pre-set enum type for self.code2key
        # from .curses_keys import code2key
        # self.code2key = code2key
        self.termstyle = TermStyle(stdscr=self.stdscr)
        return self

    def __exit__(self, *_a: object) -> None:
        self.stdscr.refresh()
        self.stdscr.nodelay(False)
        C.curs_set(self._pvis)
        self.stdscr.keypad(False)
        C.echo()
        C.noraw()
        C.endwin()

    # TBD? translate C.KEY_HOME -> universal "<Home>" str (or my common key_enum.HOME)
    #   WHY: to have the same keybindings for all clients
    def input(self) -> int | str:
        wch = self.stdscr.get_wch()
        if isinstance(wch, str):
            wch = C.unctrl(wch).decode("utf-8")
        return wch

    def sizewh(self) -> tuple[int, int]:
        # FIXME: for embedding we may want to return whatever was set during .resize(...)
        #   MAYBE: also return wx,wy for embedding offsets, or whole `Rect/`CellRect at once
        h, w = self.stdscr.getmaxyx()
        return w, h

    def draw_lines(self, lines: list[str]) -> None:  # CHG? bytes
        self.stdscr.clear()
        for s in lines:
            self.stdscr.addstr(s)
        self.stdscr.refresh()

    def draw_displ(self, displ: DisplayList) -> None:
        self.stdscr.clear()
        for token in displ:
            match token:
                case TextSpan(x, y, text, wc, aid):
                    # MAYBE: sanitize string out of interfering /\n|\r/
                    # FIXME: shortly exit fn on resize to avoid curses crash
                    #   WHY: no sense to crop frame on shrink or continue drawing on enlarge,
                    #     as displ should be recalculated for adaptive-layout anyway
                    # TEMP: disable fallback to "self.termstyle.default" to catch mismatches
                    attr = getattr(self.termstyle, Aid(aid).name)
                    self.stdscr.addnstr(y, x, text, wc, attr)
                case _:
                    assert_never(token)
        self.stdscr.refresh()
