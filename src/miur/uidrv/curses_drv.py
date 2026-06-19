import curses as C
from types import TracebackType
from typing import Callable, ClassVar, Protocol, Self, assert_never, cast, overload

from .. import log
from ..uicommon.displaylist import DisplayStream, TextSpan
from ..uicommon.styleids import Aid


class HasContext(Protocol):
    stdscr: C.window
    registered_color_pairs: dict[int, int]
    chr2attr: dict[str, int]


# TEMP: we are converting dynamic .aid solely to static fg/bg/attr cursesstyle
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
            #   FIXED? positive bg with negative fg goes into prev-bg
            fgbg = self.bg * C.COLORS * 2 + self.fg + C.COLORS
            if (curses_attr := registry.get(fgbg, None)) is None:
                i = len(registry)
                # REF: https://stackoverflow.com/questions/476878/256-colors-foreground-and-background
                #   = (65536 if has_extended_color_support() else 256)
                # REF: https://docs.python.org/3/library/curses.html#curses.color_pair
                #   BAD: Only the first 256 color pairs are supported.
                assert i < C.COLOR_PAIRS
                C.init_pair(i, self.fg, self.bg)
                curses_attr = registry[fgbg] = C.color_pair(i)
                log.info((i, fgbg))

            # ALT: next(a for nm, a in [("n": C.A_NORMAL), ...] if nm in self.attrs)
            for a in self.attrs:
                curses_attr |= instance.chr2attr[a]

        # 2. Overwrite descriptor with raw primitive int on the instance
        setattr(instance, self.name, curses_attr)
        log.info((self.name, curses_attr))
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
        log.info(f"{C.COLORS=} {C.COLOR_PAIRS=} {C.termattrs()=:b}")
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
    cursesstyle: CursesStyle
    stdscr: C.window

    def __init__(self) -> None:
        self._cleanup_callbacks: list[Callable[[], object]] = []

    def _configure(self) -> None:
        __ = self._cleanup_callbacks.append  # RENAME? _defer()
        C.setupterm()
        self.stdscr = C.initscr()
        __(C.endwin)
        C.noecho()
        __(C.echo)

        C.raw()
        __(C.noraw)

        self.stdscr.keypad(True)
        __(lambda: self.stdscr.keypad(False))

        pvis = C.curs_set(0)
        __(lambda: C.curs_set(pvis))

        ## DISABLED: currently I use blocking while-loop
        ##   ALT: py$ try: get_wch() ; except curses.error: pass; curses.napms(100)
        # self.stdscr.nodelay(True)
        __(lambda: self.stdscr.nodelay(False))

        ## BAD: maybe keys should be assigned only *after* initscr
        ##   BUT:FAIL? can't pre-set enum type for self.code2key
        # from .curses_keys import code2key
        # self.code2key = code2key
        self.cursesstyle = CursesStyle(stdscr=self.stdscr)

        # CHECK: do I even need this in cleanup?
        __(self.stdscr.refresh)

    def _restore(self) -> ExceptionGroup | None:
        errors: list[Exception] = []
        interrupts: list[BaseException] = []
        for fn in reversed(self._cleanup_callbacks):
            try:
                fn()
            except Exception as exc:
                # CASE: run all cleanups, even if some of them are failing
                #   >> we should try to restore terminal as much as we can
                errors.append(exc)
            except BaseException as cfl:
                # WARN:(BaseException): teardown even on Ctrl+C (KeyboardInterrupt) or forced exit (SystemExit)
                interrupts.append(cfl)
        if interrupts:
            # FIXME? chain them to each other (e.g. Ctrl+C during SystemExit)
            raise interrupts[0] from None
        self._cleanup_callbacks.clear()
        return ExceptionGroup("cleanup failures", errors) if errors else None

    def __enter__(self) -> Self:
        try:
            self._configure()
        except Exception as setup_exc:
            # FIXED: crash in __enter__ doesn't call __exit__
            if restore_grp := self._restore():
                # BAD: should be py:$ raise restore_grp from setup_exc
                #   BUT! we want to keep "setup_exc" as "primary error"
                raise setup_exc from restore_grp
            raise
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> None:
        restore_exc = self._restore()
        if restore_exc:
            if exc:
                ## DISABLED: we want to keep "exc" as "primary error"
                # raise restore_exc from exc
                exc.__context__ = restore_exc
            else:
                raise restore_exc

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

    def clear(self) -> None:
        self.stdscr.clear()

    def refresh(self) -> None:
        self.stdscr.refresh()

    def draw_lines(self, lines: list[str]) -> None:  # CHG? bytes
        for s in lines:
            self.stdscr.addstr(s)

    def draw_displ(self, displ: DisplayStream) -> None:
        for token in displ:
            match token:
                case TextSpan(x, y, text, wc, aid):
                    # MAYBE: sanitize string out of interfering /\n|\r/
                    # FIXME: shortly exit fn on resize to avoid curses crash
                    #   WHY: no sense to crop frame on shrink or continue drawing on enlarge,
                    #     as displ should be recalculated for adaptive-layout anyway
                    # TEMP: disable fallback to "self.cursesstyle.default" to catch mismatches
                    # TBD:OPT: for unknown styles either throw/log, fallback to default, or use toxic-pink
                    attr = getattr(self.cursesstyle, Aid(aid).name)
                    try:
                        self.stdscr.addnstr(y, x, text, wc, attr)
                    except C.error:
                        ## DISABLED:FAIL:(racing): even if we query sizewh before and after,
                        ##   window may had changed size *twice* and returned to prev size
                        ##     e.g. another window was created/destroyed on tiling WM
                        # if self.sizewh() != prevwh:
                        ## TEMP:WKRND: return early and cleanly redraw "next time"
                        ##   FIXME?: set invalidate OR issue redraw but limit number of attempts on ERR
                        ##     MAYBE? propagate and capture exception in main-loop -- to restart drawing easier
                        return
                case _:
                    assert_never(token)
