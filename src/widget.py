import os
from dataclasses import dataclass
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


@dataclass
class ABIndex:
    above: int  # .above_cursor (cursor not included)
    below: int  # .below_cursor


# FUT:SPLIT:(OLD=ScrollListWidget):
#   (ViewportXfm + CursorXfm + MultilineSyncedXfm) + DataProvider -> NaviWidget x3 -> ExploreWidget
# ARCH:
#   * WiP: allow arbitrary multiline items (i.e. viewport_height != len(visible_items))
#   * TBD: shift viewport past the cursor, FUT: keeping cursor ctx preview at the edge of viewport
#     [_] TODO: disable all destructive funcs, when cursor is out of viewport (i.e. "inactive")
# TODO: adapt to support "infinite/growing lists" and/same "list snapshots (unknown beg/end)"
class NaviWidget:  # pylint:disable=too-many-instance-attributes
    def __init__(self) -> None:
        self._lst: Sequence[Representable]
        ## MAYBE:HACK: use viewport_height_above/below like cursor/canvas already do
        ##   NICE: you don't need any separate astrangled VAR,
        ##     like self._canvas_viewport_offset for on-screen-relative position of cursor
        self._viewport = ABIndex(0, 0)  # RENAME? ._viewport_lines.{above,below}
        self._viewport_width: int = 0  # <RQ: for right-justified table items
        # WARN:(margin): should be counted in "lines" inof "items"
        #   !! orse margin over several large multiline items may even push cursor out of the viewport
        self._viewport_margin_lines: int = 0
        ## HACK: we translate index to *known/cached* lines above/below cursor inof absolute position/total
        ##   to support bi-directionally growing lists  # <RQ: for usual scrollbar UI
        # i.e. numbers above/below may change at any moment if dir contents changed,
        #   or if sliding window had loaded more contents by triggering top/bot waterline
        self._canvas = ABIndex(0, 0)  # RENAME? ._canvas_lines.{above,below}
        ## NOTE: we use bi-directionally growing "deque" inof "list index magic",
        ##   so we need to keep a pair of left-right indexes too (like with cached lines).
        ## ARCH: cursor is actually a virtual line between "above" and first item "below",
        ##   with currently picked line being "below[0]", i.e. the first item below
        ##   BUT: what if we are at the last item ? shouldn't "below" be 0 to avoid special cases ?
        self._items = ABIndex(0, 0)  # RENAME? ._items_cached.{above,below}

    ## HACK: if item under cursor had disappeared or "order-by" have changed the index,
    ##   we can temporarily re-insert the item into the list and seek for it to find a new index
    ## RQ: keep cursor in logical continuity
    # self._focused_item: Representable = None  # OLD=self._focused_item
    # NOTE:(=const): even in multiline item, we will have 1st line specifically focused
    # self._cursor_height_lines: int = 1
    # self._cursor_item_index: int = 0  # <REMOVE! should be same as "below[0]"
    # self._viewport_index: int = 0  # <REMOVE? or keep to be able to jump to first item
    # self._viewport_offset: int = 0  # <REMOVE: may point into the middle of multiline item
    @property
    def focused_item(self) -> Representable:
        if not self._lst:
            # BET? return dummy placeholder for empty dirs
            #   BAD! placeholder is *content*, it shouldn't be *focused* either
            #   ALT: always include dir itself in listing -- so we could do ops from inside the dir
            raise IndexError("empty list")
        idx = self._items.above
        ## [_] TODO?IDEA: allow cursor to be *after* last item
        #   NICE: eliminate special case for viewport/canvas
        #     CASE: for "items" if below=0, then current item is the last one from above=
        #       ~~ OR: first one from above, if we do "deque"-esque container
        #   ARCH: treat cursor as a thin line (i.e. having virtual height=0)
        #     CASE: when last item is very large multiline, placing cursor after it will represent
        #       that scrolling through whole item had finished -- and bot canvas is snapped to bot viewport.
        # if self._items.below == 0:
        #     idx -= 1
        return self._lst[idx]

    ## THINK: does .height=0 even has sense ?
    # def height(self) -> int:
    #     return (
    #         self._viewport_above_cursor
    #         + self._viewport_below_cursor
    #         + (0 if self._focused_item is None else 1)
    #     )

    def resize(self, h: int, w: int) -> None:
        self._viewport_width = w
        self._viewport_margin_lines = h // 6  # OR: fixed=2
        ### ALG: use "ratio" and scale both sides proportionally
        ## TODO: when scaling up -- hard-stick to top/bot (whichever is nearer)
        ##   &why: easier for mind to stick to expected visually fixed areas
        ##   BUT: when scaling down we still need to reduce proportionally
        ##     ALSO:TODO: align to margin, to prevent cursor jump on move after scaling down
        ## TODO:WARN:FIXME: what if cursor is outside of viewport ?
        # if oldheight := self._viewport_above_cursor + self._viewport_below_cursor:
        #     newabove = self._viewport_above_cursor * h // oldheight
        # else:
        #     ## THINK: logically we should keep the item, but then .height=1 (when it should be 0)
        #     # self._focused_item = None
        #     newabove = 0
        # self._viewport_above_cursor = newabove
        # self._viewport_below_cursor = h - self._cursor_height_lines - newabove
        ## TEMP: reset *scroll* position on -resize() [and ignore margin]
        self._viewport.above = 0
        self._viewport.below = h

    # CASE: to be able to re-assign ~same list after external xfm, e.g. after "order-by"
    def assign(self, lst: Sequence[Representable]) -> None:
        # WARN: whole function should be atomic
        #   i.e. "cursor,canvas" should always be in boundaries of "lst"
        self._lst = lst
        # TODO: if item under cursor had disappeared or "order-by" have changed the index,
        #   we can temporarily re-insert the self._focused_item into the list
        #   and seek for it to find a new index
        # TODO: pre-load only visible part fitting into viewport
        #   WARN: on first assign(), viewport height may still be =0, due to -resize() being called later
        ## TEMP: reset *cursor* position on -assign()
        self._items.above = 0
        self._items.below = len(self._lst)

    # MAYBE: make ItemWidget to calc() item height and draw it (or only ItemXfm)
    #   WARN: don't store "index" inside ItemWidget << PERF:(slow): rebuild on each order-by
    #     ~~ though, I can pass list index ctx into ItemWidget.render(ctx[index=i])
    @staticmethod
    def _itemheight(item: Representable) -> int:
        return item.name.count("\n") + 1

    # RENAME? pick_item_from_cursor_below
    def move_cursor_down(self) -> None:
        # FIXME: also scroll canvas/viewport if cursor is on last item, but it's only partially shown
        assert self._items.below > 0
        if self._items.below <= 1:
            return
        margin = self._viewport_margin_lines
        ih = self._itemheight(self.focused_item)
        self._canvas.above += ih
        self._canvas.below -= ih
        # available_offset = self._viewport.below - margin
        if self._canvas.below <= margin:
            offset = min(self._viewport.below - margin, ih)
            self._viewport.above += offset
            self._viewport.below -= offset
        elif self._viewport.below >= ih + margin:
            self._viewport.above += ih
            self._viewport.below -= ih
        else:
            raise NotImplementedError("unexpected")
            # self._viewport.above = max
            # self._viewport.below = 0

        # TODO: make fn accept other values
        delta = 1
        # [_] WARN: for large items >4 lines we need "scroll-first" strategy inof "jump-fit-next"
        self._items.above += delta
        self._items.below -= delta

    def move_cursor_up(self) -> None:
        # FIXME: also scroll canvas/viewport if cursor is on first item, but it's only partially shown
        assert self._items.above >= 0
        if self._items.above <= 0:
            return
        margin = self._viewport_margin_lines
        ih = self._itemheight(self.focused_item)
        self._canvas.above -= ih
        self._canvas.below += ih
        # available_offset = self._viewport.below - margin
        if self._canvas.above <= margin:
            offset = min(self._viewport.above - margin, ih)
            self._viewport.above -= offset
            self._viewport.below += offset
        elif self._viewport.above >= ih + margin:
            self._viewport.above -= ih
            self._viewport.below += ih
        else:
            raise NotImplementedError("unexpected")
            # self._viewport.above = 0
            # self._viewport.below = max
        # TODO: make fn accept other values
        delta = 1
        # [_] WARN: for large items >4 lines we need "scroll-first" strategy inof "jump-fit-next"
        self._items.above -= delta
        self._items.below += delta

    # RENAME:(shift)? -> "pan"
    # ALT:(merge): shift(self, *, abs=None, rel=None)
    # FUT:ALT(lines): also allow other quantities:
    #   * "items" - for whole multiline items scroll
    #   * "pixels" - for GUI smooth scrolling
    #   * "groups" - to jump by alphabet or type (or whatever current "order-by")
    def _shift_viewport_by_lines(self, lines: int, /) -> None:
        """Supports arbitrary multiline items"""
        if lines == 0 or len(self._lst) == 0:
            return

        # FIXME: I introduced _lines_above/below, to prevent unnecessary re-calculation,
        #   NEED: change code below accordingly
        raise NotImplementedError()

        # BAD? currently idx is a top item, which has at least its last line shown on screen
        # ALT:BET? treat next item as "idx", i.e. first item which has *first* line on screen
        #   NICE: in this case we can move cursor up, keeping multiline offset the same above cursor
        idx = self._viewport_index
        off = self._viewport_offset + lines

        if off > 0:
            last = len(self._lst) - 1
            # ALT: pin last item bot to the vp bot (inof last item top to the vp top)
            #   << much harder, as you need to predict when last item's last line hits vp bot
            while idx < last and off >= (ih := self._itemheight(self._lst[idx])):
                idx += 1
                off -= ih
            # ALT:HACK: allowing {off<0|off>=ih} will allow scrolling viewport past first/last item
            # ALT?NICE? wrap from lst beg if off>=ih (treat list as a ring)
            if off >= (ih := self._itemheight(self._lst[idx])):
                # NOTE: we allow scrolling until [only] last line of last item is left on the screen:
                #   = to allow displaying large items, which have more lines than fit into screen
                off = ih - 1
        elif off < 0:
            while idx > 0 and off < 0:  # pylint:disable=chained-comparison
                idx -= 1
                off += self._itemheight(self._lst[idx])
            # ALT:HACK: allowing {off<0|off>=ih} will allow scrolling viewport past first/last item
            if off < 0:  # pylint:disable=consider-using-max-builtin
                off = 0

        # assert 0 <= idx <= last
        # assert 0 <= off <= self._itemheight(self._lst[idx])
        self._viewport_index = idx
        self._viewport_offset = off

    # TODO: don't jump viewport if newindex item is still fully inside viewport,
    #   orse only scroll viewport sligtly, to show multiline item fully
    def _shift_viewport_to_index(self, index: int, /) -> None:
        if len(self._lst) == 0:
            return
        # NOTE: count negative indexes from end
        if index < 0:
            index += len(self._lst)
        last = len(self._lst) - 1
        assert 0 <= index <= last  # TEMP:QA
        self._viewport_index = max(0, min(index, last))
        self._viewport_offset = 0

    # TODO: should manifest the same behavior as advance_by_delta()
    def _shift_viewport_by_delta(self, delta: int, /) -> None:
        if delta == 0 or len(self._lst) == 0:
            return
        last = len(self._lst) - 1
        # ALT: wrap-jump cursor position
        newindex = max(0, min(self._viewport_index + delta, last))
        self._viewport_index = newindex
        # ALT: preserve offset OR pass optional :offset together with :delta
        self._viewport_offset = 0

    def _move_cursor_to_index(self, index: int, /) -> None:
        # THINK: replace by assert and prevent situation from outside ?
        if len(self._lst) == 0:
            return
        # NOTE: count negative indexes from end
        if index < 0:
            index += len(self._lst)
        last = len(self._lst) - 1
        assert 0 <= index <= last  # TEMP:QA
        self._cursor_item_index = max(0, min(index, last))

    def _move_cursor_by_delta(self, delta: int, /) -> None:
        if delta == 0 or len(self._lst) == 0:
            return
        last = len(self._lst) - 1
        # ALT: wrap-jump cursor position
        newindex = max(0, min(self._cursor_item_index + delta, last))
        self._cursor_item_index = newindex

    def _move_cursor_to_viewport_pos(self, pos: int, /) -> None:
        # ex~: jump to 7th item on screen (inof 7th from list beginning)
        # THINK: "pos" as in "lines from top" or "item index from viewport start" ?
        # ALG: translate "pos" into "index"
        raise NotImplementedError()

    def _scroll_to_index(self, index: int, /) -> None:
        # TODO: heuristics on adjusting viewport based on jump dir (prev cursor pos)
        # ex~: restore view when re-entering some previous directory
        raise NotImplementedError()

    # ALT:SPLIT: "scroll_by_lines" for smooth (scroll offset -> then move index),
    #   and "refocus_by_delta" for snapping (adjust viewport to fit item's first line)
    def _advance_by_delta(self, delta: int, /) -> None:
        """Supports arbitrary multiline items"""
        # CASE: normal j/k navigation
        newindex = self._cursor_item_index + delta
        raise NotImplementedError()

        ## BAD:FIXME: should work for arbitrary-length multilines
        margin = self._viewport_margin_lines
        first, last = 0, len(self._lst) - 1
        top = first + margin
        off = self._viewport_index  # ~ first visible item
        upp = off + margin
        h = self._viewport_height_lines
        # FIXME: when {h/vi < margin*2}
        low = off + h - margin - 1
        mst = last - h + 1
        bot = last - margin

        # CHG? &next actually call dedicated fns above
        # if newindex is None:  # REMOVE:HACK: jump to last element
        #     vi, ci = mst, last
        if newindex < first:
            vi, ci = first, first
        elif newindex < top:
            vi, ci = first, newindex
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
        self._cursor_item_index = ci
        # DEBUG: print(' '.join(f"{k}={v}" for k, v in locals().items() if k not in ('self', 'wg')))

    def _center_viewport_on_cursor(self) -> None:
        # TODO:OPT: mid/first/last-offset/margin
        # CASE: pressing 'j/k' after shifting viewport outside cursor area
        # CASE: restoring NaviWidget state after re-entering previously visited directory
        raise NotImplementedError()

    def _center_cursor_on_viewport(self) -> None:
        # TODO:OPT: mid/first/last-offset/margin
        # CASE: pressing 'l' (to select) after shifting viewport outside cursor area
        # CASE: if you decided to abandon previous cursor position and need to preview smth in shifted area
        # TODO: jumplist history <C-o> -- to return to previous cursor position
        raise NotImplementedError()

    def redraw(self, stdscr: C.window) -> None:
        # def _pfx(i: int) -> str:
        #     idx = 1 + i + vctx.wndabsoff0
        #     cur = ">" if i == vctx.wndcurpos0 else ":"
        #     return f"{1+i:02d}| {idx:03d}{cur} "
        c_item = C.color_pair(ColorMap.default)
        c_auxinfo = C.color_pair(ColorMap.auxinfo)
        c_cursor = C.A_REVERSE | C.A_BOLD  # OR: C.color_pair(ColorMap.cursor)

        # stdscr.move(self._viewport.above, 0)
        cursor_y = self._viewport.above
        text = self.focused_item.name
        stdscr.addstr(cursor_y, 3, text, c_cursor)

        ## TODO: draw interlacingly one from below / one from above (based on prev move direction)
        ##   &why: much better usage-pattern/responsiveness on very slowly updating screens
        idx = self._items.above
        y = self._viewport.above
        while idx > 0:
            idx -= 1
            item = self._lst[idx]
            ih = self._itemheight(item)
            y -= ih
            # TEMP: only draw fully-fitting multiline items
            #   WARN! we assume that: { top of NaviWidget = top of RootWidget = 0,0 }
            if y < 0:  # OR? self._canvas.above
                break
            stdscr.addstr(y, 3, item.name, c_item)

        ## TEMP:WARN: we assume non-empty list
        last = self._items.above + 1 + self._items.below
        idx = self._items.above + 1  # NOTE: skip cursor, start from next item
        ih = self._itemheight(self.focused_item)
        # HACK: we need to skip previously drawn cursor too
        y = self._viewport.above + ih
        while idx < last:
            item = self._lst[idx]
            ih = self._itemheight(item)
            # TEMP: only draw fully-fitting multiline items
            #   WARN! we assume that: { top of NaviWidget = top of RootWidget = 0,0 }
            if (
                y + ih >= self._viewport.above + self._viewport.below
            ):  # OR? self._canvas.below
                break
            stdscr.addstr(y, 3, item.name, c_item)
            y += ih
            idx += 1


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
        # return repr(self._x)
        # TEMP:TEST: multiline entries
        return str(self._x).replace("n", "n⬎\n")

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
    _wdg: NaviWidget

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
            # TEMP:TRY:
            self._wdg = NaviWidget()
            self._wdg.assign(self._lst)

    def cursor_move_rel(self, modifier: int) -> None:
        # c = self._vctx
        # newpos = c.wndcurpos0 + modifier
        # c.wndcurpos0 = max(min(newpos, c.wndmaxlen - 1, len(self._lst) - 1), 0)
        if modifier < 0:
            self._wdg.move_cursor_up()
        elif modifier > 0:
            self._wdg.move_cursor_down()
        else:
            raise NotImplementedError("should refresh")

    def resize(self, stdscr: C.window) -> None:
        self._wdg.resize(*stdscr.getmaxyx())
        self.redraw(stdscr)

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)

        # c = self._vctx
        # c.wdgh, c.wdgw = stdscr.getmaxyx()
        # log.info(f"draw: [{c.wdgh}x{c.wdgw}]")
        # FIXME: "-1" should be externally calculated by `Layout, based on "Footer.height"
        # c.wndmaxlen = max(c.wdgh - 1, 0)
        # wndabsoff0: beg, _end = self._wg._scroll.range(i)
        # wndcurpos0: pos = self._wg.currel
        # FIXED: prevent crash when window shrinks past the cursor
        # self.cursor_move_rel(0)

        # NOTE: actually _lst here stands for a generic _augdbpxy with read.API
        #   i.e. DB augmented by virtual entries, all generated-and-cleared on demand
        # draw_list(stdscr, self._lst, c)
        # draw_footer(stdscr)
        self._wdg.redraw(stdscr)

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
