import os
from typing import Callable, Iterable, Protocol, Sequence, TypeVar, override

import _curses as C

from .curses_ext import ColorMap
from .util.logger import log


class Representable(Protocol):
    @property
    def name(self) -> str: ...


# ALSO: my earlier View concept is actually a Presenter
#   : VisibleContext=ScrollWindow+Cursor+Selections+OverlayHints+Tags+etc.
class VisibleContext:
    wdgh: int
    wdgw: int
    wndabsoff0: int
    wndmaxlen: int
    wndcurpos0: int


# FUT:SPLIT:(OLD=ScrollListWidget):
#   (ViewportXfm + CursorXfm + MultilineSyncedXfm) + DataProvider -> NaviWidget x3 -> ExploreWidget
# ARCH:
#   * WiP: allow arbitrary multiline items (i.e. viewport_height != len(visible_items))
#   * TBD: shift viewport past the cursor, FUT: keeping cursor ctx preview at the edge of viewport
#     [_] TODO: disable all destructive funcs, when cursor is out of viewport (i.e. "inactive")
class NaviWidget:
    def __init__(self, lst: Sequence[Representable]) -> None:
        # TODO: adapt to support "infinitely growing lists" and/same "list snapshots (unknown beg/end)"
        self._lst = lst
        self._viewport_height: int = 0
        self._viewport_width: int = 0  # = needed for right-justified table items
        self._viewport_index: int = 0
        self._viewport_offset: int = 0  # = may point into the middle of multiline item
        self._cursor_index: int = 0
        # WARN:(margin): should be counted in "lines" inof "items"
        #   !! orse margin over several large multiline items may even push cursor out of the viewport
        self._viewport_margin_lines: int = 0

    def resize(self, h: int, w: int) -> None:
        self._viewport_height = h
        self._viewport_width = w
        self._viewport_margin_lines = h // 6  # OR: fixed=2

    # RENAME:(shift)? -> "pan"
    # ALT:(merge): shift(self, *, abs=None, rel=None)
    # FUT:ALT(lines): also allow other quantities:
    #   * "items" - for whole multiline items scroll
    #   * "pixels" - for GUI smooth scrolling
    #   * "groups" - to jump by alphabet or type (or whatever current "order-by")
    def shift_viewport_by_lines(self, lines: int, /) -> None:
        if lines == 0 or len(self._lst) == 0:
            return

        # MAYBE: make ItemWidget to calc() item height and draw it (or only ItemXfm)
        #   WARN: don't store "index" inside ItemWidget << PERF:(slow): rebuild on each order-by
        #     ~~ though, I can pass list index ctx into ItemWidget.render(ctx[index=i])
        def itemheight(index: int) -> int:
            return self._lst[index].name.count("\n") + 1

        # BAD? currently idx is a top item, which has at least its last line shown on screen
        # ALT:BET? treat next item as "idx", i.e. first item which has *first* line on screen
        #   NICE: in this case we can move cursor up, keeping multiline offset the same above cursor
        idx = self._viewport_index
        off = self._viewport_offset + lines

        if off > 0:
            last = len(self._lst) - 1
            # ALT: pin last item bot to the vp bot (inof last item top to the vp top)
            #   << much harder, as you need to predict when last item's last line hits vp bot
            while idx < last and off >= (ih := itemheight(idx)):
                idx += 1
                off -= ih
            # ALT:HACK: allowing {off<0|off>=ih} will allow scrolling viewport past first/last item
            # ALT?NICE? wrap from lst beg if off>=ih (treat list as a ring)
            if off >= (ih := itemheight(idx)):
                # NOTE: we allow scrolling until [only] last line of last item is left on the screen:
                #   = to allow displaying large items, which have more lines than fit into screen
                off = ih - 1
        elif off < 0:
            while idx > 0 and off < 0:  # pylint:disable=chained-comparison
                idx -= 1
                off += itemheight(idx)
            # ALT:HACK: allowing {off<0|off>=ih} will allow scrolling viewport past first/last item
            if off < 0:  # pylint:disable=consider-using-max-builtin
                off = 0

        # assert 0 <= idx <= last
        # assert 0 <= off <= itemheight(idx)
        self._viewport_index = idx
        self._viewport_offset = off

    def shift_viewport_to_index(self, index: int, /) -> None:
        if len(self._lst) == 0:
            return
        # NOTE: count negative indexes from end
        if index < 0:
            index += len(self._lst)
        last = len(self._lst) - 1
        assert 0 <= index <= last  # TEMP:QA
        self._viewport_index = max(0, min(index, last))
        self._viewport_offset = 0

    def shift_viewport_by_delta(self, delta: int, /) -> None:
        if delta == 0 or len(self._lst) == 0:
            return
        last = len(self._lst) - 1
        # ALT: wrap-jump cursor position
        newindex = max(0, min(self._viewport_index + delta, last))
        self._viewport_index = newindex
        # ALT: preserve offset OR pass optional :offset together with :delta
        self._viewport_offset = 0

    def move_cursor_to_index(self, index: int, /) -> None:
        # THINK: replace by assert and prevent situation from outside ?
        if len(self._lst) == 0:
            return
        # NOTE: count negative indexes from end
        if index < 0:
            index += len(self._lst)
        last = len(self._lst) - 1
        assert 0 <= index <= last  # TEMP:QA
        self._cursor_index = max(0, min(index, last))

    def move_cursor_by_delta(self, delta: int, /) -> None:
        if delta == 0 or len(self._lst) == 0:
            return
        last = len(self._lst) - 1
        # ALT: wrap-jump cursor position
        newindex = max(0, min(self._cursor_index + delta, last))
        self._cursor_index = newindex

    def move_cursor_to_viewport_pos(self, pos: int, /) -> None:
        # ex~: jump to 7th item on screen (inof 7th from list beginning)
        # THINK: "pos" as in "lines from top" or "item index from viewport start" ?
        # ALG: translate "pos" into "index"
        raise NotImplementedError()

    def scroll_to_index(self, index: int, /) -> None:
        # TODO: heuristics on adjusting viewport based on jump dir (prev cursor pos)
        # ex~: restore view when re-entering some previous directory
        raise NotImplementedError()

    # ALT:SPLIT: "scroll_by_lines" for smooth (scroll offset -> then move index),
    #   and "refocus_by_delta" for snapping (adjust viewport to fit item's first line)
    def refocus_by_delta(self, delta: int, /) -> None:
        # CASE: normal j/k navigation
        newindex = self._cursor_index + delta
        raise NotImplementedError()

        ## BAD:FIXME: should work for arbitrary-length multilines
        margin = self._viewport_margin_lines
        beg = 0
        top = beg + margin
        off = self._viewport_index  # ~ first visible item
        upp = off + margin
        h = self._viewport_height
        # FIXME: when {h/vi < margin*2}
        low = off + h - margin - 1
        last = len(self._lst) - 1
        mst = last - h + 1
        bot = last - margin

        # CHG? &next actually call dedicated fns above
        # if newindex is None:  # REMOVE:HACK: jump to last element
        #     vi, ci = mst, last
        if newindex < beg:
            vi, ci = beg, beg
        elif newindex < top:
            vi, ci = beg, newindex
        elif newindex < upp:
            vi, ci = (newindex - margin), newindex
        elif newindex <= low:
            vi, ci = off, newindex
        elif newindex <= bot:
            vi, ci = (newindex - h + margin + 1), newindex
        elif newindex <= last:
            vi, ci = mst, newindex
        else:
            vi, ci = mst, last
        self._viewport_index = vi
        self._cursor_index = ci
        # DEBUG: print(' '.join(f"{k}={v}" for k, v in locals().items() if k not in ('self', 'wg')))


def draw_list(
    stdscr: C.window, lst: Sequence[Representable], vctx: VisibleContext
) -> None:
    log.verbose(f"list: [<={vctx.wndmaxlen}/{len(lst)}]")

    def _pfx(i: int) -> str:
        idx = 1 + i + vctx.wndabsoff0
        cur = ">" if i == vctx.wndcurpos0 else ":"
        return f"{1+i:02d}| {idx:03d}{cur} "

    citem = C.color_pair(ColorMap.default)
    caux = C.color_pair(ColorMap.auxinfo)
    ccurs = C.A_REVERSE | C.A_BOLD  # OR: C.color_pair(ColorMap.cursor)

    def _draw_item_at(i: int, attr: int) -> None:
        idx = i + vctx.wndabsoff0
        pfx = _pfx(i)
        stdscr.addstr(i, 0, pfx, caux)
        text = lst[idx].name
        stdscr.addstr(text, attr)
        # log.verbose(f"{pfx}{text}")  # :{attr}:

    for i in range(0, min([vctx.wndmaxlen, len(lst)])):
        _draw_item_at(i, citem)

    ## NOTE: draw cursor AGAIN after footer (i.e. over-draw on top of full list)
    ##   NICE: no need to hassle with storing cursor prefix length for cx/cy
    ##   NICE: can redraw only two lines (prev item and cursor) inof whole list
    # cx = len(_pfx(vctx.wndcurpos0))
    # _draw_item_at(vctx.wndcurpos0, citem)
    # stdscr.move(vctx.wndcurpos0, cx)
    ## OR:BET: only change attributes of already printed line
    cx = len(_pfx(vctx.wndcurpos0))
    cn = len(lst[vctx.wndcurpos0 + vctx.wndabsoff0].name)
    stdscr.chgat(vctx.wndcurpos0, cx, cn, ccurs)


# def draw_footer(stdscr: C.window) -> None:
#     pair = 30
#     C.init_pair(pair, 217, 17)
#     attr = C.color_pair(pair)
#     # info = C.color_content(fg)
#     # info = C.COLOR_PAIRS
#     #
#     # # pylint:disable=protected-access
#     # pv = self._wg._scroll._provider
#     # idx = 1 + self._wg.curabs
#     # sortby = pv._sortby
#     # sortrev = "￬" if pv._sortrev else "￪"
#     # info = f"{idx}/{len(pv)} | {sortby=}{sortrev}"
#     info = 100
#
#     hh, _ww = stdscr.getmaxyx()  # C.LINES
#     stdscr.addstr(hh - 1, 0, f"--- {info}", attr)


class FSEntry(Representable):
    def __init__(self, path: str) -> None:
        self._x = path

    @override
    @property
    def name(self) -> str:
        return repr(self._x)

    # i.e. =InterpretUnchangedDirListingPropertyAsFSEntriesInUsualWay
    def explore(self) -> Iterable["FSEntry"]:
        with os.scandir(self._x) as it:
            return [FSEntry(e.path) for e in it]


# T = TypeVar("T")
# class ListCachingProxy(list[T]):
#     pass


class RootWidget:
    _ent: Representable
    _lst: Sequence[Representable]
    # _lstpxy: ListCachingProxy[Representable]
    _act: Callable[[], Sequence[Representable]]
    _vctx: VisibleContext

    def set_entity(self, ent: Representable) -> None:
        c = VisibleContext()
        c.wndmaxlen = 0  # WKRND:BAD: we don't have stdscr here to getmaxyx()
        c.wndabsoff0 = 0
        c.wndcurpos0 = 0  # MAYBE: keep prev cursor position if ent refers to same loci
        self._vctx = c
        self._ent = ent
        if (sfn := getattr(ent, "explore")) and callable(sfn):
            self._act = sfn  # NOTE: keep sfn to be able to refresh() the list (when externally changed)
            self._lst = self._act()

    def cursor_move_rel(self, modifier: int) -> None:
        c = self._vctx
        newpos = c.wndcurpos0 + modifier
        c.wndcurpos0 = max(min(newpos, c.wndmaxlen - 1, len(self._lst) - 1), 0)

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)

        c = self._vctx
        c.wdgh, c.wdgw = stdscr.getmaxyx()
        log.info(f"draw: [{c.wdgh}x{c.wdgw}]")
        # FIXME: "-1" should be externally calculated by `Layout, based on "Footer.height"
        c.wndmaxlen = max(c.wdgh - 1, 0)
        # wndabsoff0: beg, _end = self._wg._scroll.range(i)
        # wndcurpos0: pos = self._wg.currel
        # FIXED: prevent crash when window shrinks past the cursor
        self.cursor_move_rel(0)

        # NOTE: actually _lst here stands for a generic _augdbpxy with read.API
        #   i.e. DB augmented by virtual entries, all generated-and-cleared on demand
        draw_list(stdscr, self._lst, c)
        # draw_footer(stdscr)

    # USE: log.info(str(wdg))
    # def __str__(self) -> str:
    #     s = "  " * 0 + str(0) + ": " + self._ent.name
    #     for i, x in enumerate(self._lstpxy, start=1):
    #         s += "\n  " * 1 + str(i) + ": " + x.name
    #     s += "\r"
    #     s += str(v) if isinstance((v := self._valpxy.get()), int) else repr(v)
    #     return s


def _live() -> None:
    log.sep()
    from .app import g_app as g
    from .widget import RootWidget

    g.root_wdg = wdg = RootWidget()
    # wdg.set_entity(FSEntry("/etc/udev"))
    wdg.set_entity(FSEntry("/d/airy"))
    g.curses_ui.resize()
    # wdg.redraw(g.stdscr)
    # g.stdscr.refresh()
