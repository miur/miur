import curses as C
from types import TracebackType
from typing import Callable, Self, assert_never

from .. import log
from ..uicommon.displaylist import DisplayStream, TextSpan
from ..uicommon.styleids import Aid, StyleId
from ..uicommon.stylesheet import ColorIndex, UnitedStylesheet


# SEP/OPT::
#   * fullscreen (vs embedded piece-of-screen)
#   * curses (vs my own native-tui) (vs Textual/Rich/Asciimatics/Blessed/Urwid/PromptToolkit)
#     - Textual: The most popular, modern async framework for Python terminal layouts.
#         It relies on standard CSS formatting and natively handles 24-bit colors.
#     - Rich: A lighter utility for rich text styling, true-color logging, tables, and terminal formatting.
#     - Asciimatics: Great if you need animations or 24-bit graphics running directly inside your terminal.
#   * TBD: webapp/pygame
class CursesUIDriver:
    stdscr: C.window
    registered_color_pairs: dict[int, int]
    style_by_id: list[int]

    def __init__(self) -> None:
        self._cleanup_callbacks: list[Callable[[], object]] = []

    # RENAME? _intern_fgbg
    def _encode_fgbg(self, fg: ColorIndex, bg: ColorIndex) -> int:
        # WARN: can't be static/standalone function due to C.COLORS only known in runtime
        assert -1 <= fg < C.COLORS
        assert -1 <= bg < C.COLORS
        return ((bg + 2) << 9) | (fg + 2)

    def resolve_style(self, aid: StyleId) -> int:
        c = UnitedStylesheet.resolve_by_aid(aid)
        fg = -1 if c.fg is None else c.fg
        bg = -1 if c.bg is None else c.bg
        fgbg = self._encode_fgbg(fg, bg)
        registry = self.registered_color_pairs
        if (curses_attr := registry.get(fgbg, None)) is None:
            i = len(registry)
            # REF: https://stackoverflow.com/questions/476878/256-colors-foreground-and-background
            #   = (65536 if has_extended_color_support() else 256)
            # REF: https://docs.python.org/3/library/curses.html#curses.color_pair
            #   BAD: Only the first 256 color pairs are supported.
            assert i < C.COLOR_PAIRS, "TBD: auto-cleanup colorpairs from hidden screens"
            C.init_pair(i, fg, bg)
            curses_attr = registry[fgbg] = C.color_pair(i)
        # FIXME: error-out (or PINK or DFL) if any other keyword
        if c.bold:
            curses_attr |= C.A_BOLD
        if c.italic:
            curses_attr |= C.A_ITALIC
        return curses_attr

    def _init_color_support(self) -> None:
        # FIXME: pick different fallbacks/derivations based on number of allowed colors
        if not C.has_extended_color_support():
            raise NotImplementedError
        C.start_color()
        C.use_default_colors()
        log.info(f"{C.COLORS=} {C.COLOR_PAIRS=} {C.termattrs()=:b}")
        assert C.COLORS == (1 << 8), "FIXME? _encode_fgbg(): bg<<log2(C.COLORS)"
        # FIXED: init_pair(-1,-1)==0 always exist (and reset by .use_default_colors())
        self.registered_color_pairs = {self._encode_fgbg(-1, -1): 0}
        self.style_by_id = []

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
        self._init_color_support()

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
        # HACK:(hotreload): reset style-map on each frame to refresh after jurigged DEV
        #   << COS we still need cache as it's too much to resolve style per each displ token
        #   BET:FUT: reset it only on-demand by signal(class.xpath) from jurigged itself
        #   ALT:(straightforward troubleshooting): pre-create all caches on startup (PERF?)
        # (orse):BAD: cache preserves value even if member is deleted from Aid
        #   WARN:(hides errors): cache will continue to accept DisplayList with old ids
        if __debug__:
            self.style_by_id = []
        styles = self.style_by_id

        for token in displ:
            match token:
                case TextSpan(x, y, text, wc, aid):
                    # MAYBE: sanitize string out of interfering /\n|\r/
                    #   ~~ we already do that on server, but we may need to do that AGAIN -- for predictable UI
                    # FIXME: shortly exit fn on resize to avoid curses crash
                    #   WHY: no sense to crop frame on shrink or continue drawing on enlarge,
                    #     as displ should be recalculated for adaptive-layout anyway
                    if Aid.sanitize(aid) >= len(styles):
                        styles += [-1] * (aid + 1 - len(styles))
                    if (curses_attr := styles[aid]) < 0:
                        curses_attr = styles[aid] = self.resolve_style(aid)
                    try:
                        self.stdscr.addnstr(y, x, text, wc, curses_attr)
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
