import curses as C
from contextlib import ExitStack
from types import TracebackType
from typing import TYPE_CHECKING, Self, assert_never

from wcwidth import width

from .. import log
from ..uicommon.displaylist import DisplayStream, TextSpan
from ..uicommon.styleids import Aid, StyleId
from ..uicommon.stylesheet import ColorIndex, Effect, UnitedStylesheet


# SEP/OPT::
#   * fullscreen (vs embedded piece-of-screen)
#   * curses (vs my own native-tui) (vs Textual/Rich/Asciimatics/Blessed/Urwid/PromptToolkit)
#     - Textual: The most popular, modern async framework for Python terminal layouts.
#         It relies on standard CSS formatting and natively handles 24-bit colors.
#     - Rich: A lighter utility for rich text styling, true-color logging, tables, and terminal formatting.
#     - Asciimatics: Great if you need animations or 24-bit graphics running directly inside your terminal.
#   * TBD: webapp/pygame
class CursesUIDriver:
    _stack: ExitStack
    stdscr: C.window
    registered_color_pairs: dict[int, int]
    int2key: dict[int, str]
    style_by_id: list[Effect[int] | None]

    # RENAME? _intern_fgbg
    def _encode_fgbg(self, fg: ColorIndex, bg: ColorIndex) -> int:
        # WARN: can't be static/standalone function due to C.COLORS only known in runtime
        assert -1 <= fg < C.COLORS
        assert -1 <= bg < C.COLORS
        return ((bg + 2) << 9) | (fg + 2)

    def _register_color(self, fg: ColorIndex | None, bg: ColorIndex | None) -> int:
        # FAIL: "None" may indicate a BUG(forgotten fg/bg) and NEED for toxic-pink fallback
        fg = -1 if fg is None else fg
        bg = -1 if bg is None else bg
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
        return curses_attr

    def resolve_style(self, aid: StyleId) -> Effect[int]:
        c = UnitedStylesheet.resolve_by_aid(aid)
        # FIXME: error-out (or toxic-PINK or DFL) if any other (yet unsupported) keyword
        curses_attr = 0
        if c.bold:
            curses_attr |= C.A_BOLD
        if c.italic:
            curses_attr |= C.A_ITALIC
        if c.uline:
            curses_attr |= C.A_UNDERLINE

        if c.effn:
            text2idx = c.effn

            def _effect(text: str) -> int:
                idx = text2idx(text)
                if isinstance(c.fg, (tuple, list)):
                    fg = c.fg[idx % len(c.fg)]
                else:
                    fg = idx  # TEMP:HACK: use idx as termcolor index
                if isinstance(c.bg, (tuple, list)):
                    bg = c.bg[idx % len(c.bg)]
                else:
                    bg = -1
                return curses_attr | self._register_color(fg, bg)

            return _effect

        assert isinstance(c.fg, int) and isinstance(c.bg, int)
        curses_attr |= self._register_color(c.fg, c.bg)
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

    def _configure(self, stack: ExitStack) -> None:
        __ = stack.callback
        C.setupterm()
        self.stdscr = C.initscr()
        __(C.endwin)
        C.noecho()
        __(C.echo)

        ## FAIL: doesn't preserve "\n->\r\n" unlike tty.setcbreak(fd)
        #   :only: interrupt, quit, suspend, and flow control
        # C.cbreak(); __(C.nocbreak)
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
        self.int2key = {getattr(C, k): k for k in dir(C) if k.startswith("KEY_")}

        ## BAD: maybe keys should be assigned only *after* initscr
        ##   BUT:FAIL? can't pre-set enum type for self.code2key
        # from .curses_keys import code2key
        # self.code2key = code2key
        self._init_color_support()

        # CHECK: do I even need this in cleanup?
        __(self.stdscr.refresh)

    def __enter__(self) -> Self:
        with ExitStack() as stack:
            self._configure(stack)
            self._stack = stack.pop_all()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> bool | None:
        return self._stack.__exit__(exc_type, exc_val, exc_tb)

    # TBD? translate C.KEY_HOME -> universal "<Home>" str (or my common key_enum.HOME)
    #   WHY: to have the same keybindings for all clients
    def input(self) -> str:
        wch = self.stdscr.get_wch()
        # log.debug(f"C.{wch=}")
        if wch == C.ERR:
            pass  # TBD
        if isinstance(wch, str):
            wch = C.unctrl(wch).decode("utf-8")
        elif nm := self.int2key.get(wch, ""):
            wch = nm + f"({wch})"
        else:
            wch = str(wch)
        # if isinstance(wch, str) and len(wch) == 1 and wch.islower() and wch.isalpha():
        #     return wch
        # return self.translate_to_unified_input(wch)
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

    def draw_status(self, text: str) -> None:
        wnd_w, wnd_h = self.sizewh()
        if wnd_h > 0:
            tw = min(wnd_w, width(text))
            tok = TextSpan(0, 0, text, tw, Aid.FOOTER)
            self.draw_displ([tok._replace(y=wnd_h - 1)])

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
                    if Aid.sanitize(aid) >= len(styles):
                        styles += [None] * (aid + 1 - len(styles))
                    if (eff := styles[aid]) is None:
                        eff = styles[aid] = self.resolve_style(aid)
                    # FAIL: for multi-colors requires xfm to multiple tokens
                    curses_attr = eff(text) if callable(eff) else eff
                    try:
                        # MAYBE: sanitize string out of interfering /\n|\r/ for predictable UI
                        #   ~~ we already do that on server, but we may need to do that AGAIN
                        # FIXME: shortly exit fn on resize to avoid curses crash
                        #   WHY: no sense to crop frame on shrink or continue drawing on enlarge,
                        #     as displ should be recalculated for adaptive-layout anyway
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


if TYPE_CHECKING:
    from .base_drv import BaseUIDriver

    _instance: BaseUIDriver = CursesUIDriver()
    _factory: type[BaseUIDriver] = CursesUIDriver
