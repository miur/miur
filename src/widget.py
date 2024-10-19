import os
from typing import Callable, Iterable, Protocol, Sequence, override

import _curses as C

from .curses_ext import ColorMap
from .util.logger import log


class Representable(Protocol):
    @property
    def name(self) -> str: ...


class NaviWidget:  # pylint:disable=too-many-instance-attributes
    def __init__(self) -> None:
        self._lst: Sequence[Representable]
        # ARCH:
        #  * when "viewport follows cursor", then followeditem==item_under_cursor,
        #    with offset being the same as for cursor itself
        #    >> NICE: even if items around cursor will be removed/inserted/changed,
        #      viewport will stay in same place relative to item under cursor
        #  * when "viewport freely scrolls", then followeditem==top/bot item in viewport,
        #    based on direction of the current scroll, with offset sticking to that item
        self._viewport_followeditem_lstindex = 0
        # NOTE: will become negative *only* when scrolling past first line of last multiline item
        self._viewport_followeditem_linesfromtop = 0
        self._viewport_height_lines = 0
        self._viewport_width_columns = 0  # <RQ: for right-justified table items
        # WARN:(margin): should be counted in "lines" inof "items"
        #   !! orse margin over several large multiline items may even push cursor out of the viewport
        self._viewport_margin_lines = 0
        self._cursor_item_lstindex = 0

    # ARCH: when we have multiple cursors "focused_item" is the item under currently active cursor
    #    MAYBE:THINK: use .subfocus(canvas_line/word) to apply actions to specific auxinfo of focused item
    @property
    def focused_item(self) -> Representable:
        if not self._lst:
            # BET? return dummy placeholder for empty dirs
            #   BAD! placeholder is *content*, it shouldn't be *focused* either
            #   ALT: always include dir itself in listing -- so we could do ops from inside the dir
            raise IndexError("empty list")
        return self._lst[self._cursor_item_lstindex]

    def resize(self, h: int, w: int) -> None:
        ph = self._viewport_height_lines
        self._viewport_height_lines = h
        self._viewport_width_columns = w
        self._viewport_margin_lines = h // 6  # OR: fixed=2
        # KEEP: self._viewport_followeditem_lstindex
        # TODO: adjust resulting offset to align onto margin
        if ph > 0 and (ratio := self._viewport_followeditem_linesfromtop / ph) > 0:
            self._viewport_followeditem_linesfromtop = int(h * ratio)

    # CASE:(lightweight): to be able to re-assign ~same list after external xfm, e.g. after "order-by"
    def assign(self, lst: Sequence[Representable]) -> None:
        pidx = self._cursor_item_lstindex
        focused = self._lst[pidx]
        # WARN: whole function should be atomic
        #   i.e. "cursor,canvas" should always be in boundaries of "lst"
        # TODO: pre-load only visible part fitting into viewport
        #   WARN: on first assign(), viewport height may still be =0, due to -resize() being called later
        self._lst = lst
        # TODO: if item under cursor had disappeared we can temp-reinsert the _focused_item into the list
        #   and seek for it to find a new index, then pick item before or after expected position
        # NOTE: search for the item if "order-by" have changed its index
        ## TEMP: reset *cursor* position on -assign()
        if focused is not lst[pidx]:
            try:
                newidx = lst.index(focused)
            except ValueError:
                newidx = 0
            self._cursor_item_lstindex = newidx
            self._viewport_followeditem_lstindex = newidx
            # KEEP: self._viewport_followeditem_linesfromtop

    # MAYBE: make ItemWidget to calc() item height and draw it (or only ItemXfm)
    #   WARN: don't store "index" inside ItemWidget << PERF:(slow): rebuild on each order-by
    #     ~~ though, I can pass list index ctx into ItemWidget.render(ctx[index=i])
    #   NICE:IDEA: make scrollbar ~imprecise/elastic~ and scroll through *items* when jumping far,
    #     but scroll by lines when scrolling around current viewport, where we already know all items heights
    @staticmethod
    def _itemheight(item: Representable) -> int:
        return item.name.count("\n") + 1

    # NOTE: elastic scroll, where "step" is anything between 1 line/word or whole multiline item,
    #   depending on what less disrupts the perception flow
    def step_by(self, steps: int) -> None:
        if steps in (-1, 1):
            # TODO:ALG: for large items >4 lines we need "scroll-first" strategy inof "jump-fit-next"
            # focused = self._lst[self._cursor_item_lstindex]
            # ih = self._itemheight(focused)
            # if ih > 4:
            #     TODO_scroll_by(3)_OR_keep_at_margin()
            #     if total_offset > ih:
            #         idx += 1
            # else:
            #     TODO_scroll_by(ih)_OR_keep_at_margin()
            #     idx += 1
            # TODO: scroll canvas/viewport if cursor is on first/last item, but it's only partially shown
            newidx = (pidx := self._cursor_item_lstindex) + steps
            self._viewport_followeditem_lstindex = self._cursor_item_lstindex = newidx

            rng = range(min(pidx, newidx), max(pidx, newidx))
            hlines = sum(self._itemheight(self._lst[i]) for i in rng)
            # RENAME: delta/advance/shift
            offset = -hlines if steps < 0 else hlines
            # TEMP:FAIL: can't scroll, list is limited to the size of viewport
            # TODO: offset=0 if linesfromtop < margin or linesfromtop > h - margin
            self._viewport_followeditem_linesfromtop += offset
        else:
            raise NotImplementedError("TEMP:WiP")

    def move_cursor_by(self, delta: int) -> None:
        xs = self._items
        if xs.above < 0 or xs.below <= 1 or (xs.above + xs.below) != len(self._lst):
            # return
            raise RuntimeError(xs)
        idx = xs.above = (pidx := xs.above) + delta
        xs.below -= delta
        rng = range(min(pidx, idx), max(pidx, idx))
        hlines = sum(self._itemheight(self._lst[i]) for i in rng)
        self.scroll_by(-hlines if delta < 0 else hlines)

    def scroll_by(self, advance: int) -> None:
        cs, vp = self._canvas, self._viewport
        # WARN: "vp" can be negative, when cursor is outside of vp
        #   TODO: assert if viewport is still inside allowed boundaries
        #     =i.e. max "one screen minus one item" above/below the canvas
        if cs.above < 0 or cs.below < 0 or (cs.above + cs.below) < len(self._lst):
            raise RuntimeError(cs)

        # WARN: if vp at bot -- we can modify only vp, keeping cs the same (as cursor is fixed)
        #   = "true scroll" w/o moving cursor
        cs.above += advance
        cs.below -= advance

        margin = self._viewport_margin_lines
        # available_offset = vp.below - margin
        # FIXME: mind-bogging mess
        if advance > 0:
            if cs.below <= margin:
                # RENAME: residue / alignment
                offset = min(vp.below - margin, advance)
            elif vp.below >= advance + margin:
                offset = advance
            else:
                offset = vp.below
                # raise NotImplementedError("unexpected")
        else:
            if cs.above <= margin:
                offset = -min(vp.above - margin, advance)
            elif vp.above >= advance + margin:
                offset = -advance
            else:
                offset = -vp.above
                # raise NotImplementedError("unexpected")
        vp.above += offset
        vp.below -= offset

    def redraw(self, stdscr: C.window) -> None:
        # draw_list(stdscr, self._lst, c)
        # draw_footer(stdscr)
        # ARCH:WARN: we actually need to render whatever is *shown in viewport* (even if cursor is far outside)
        #   COS: when cursor is outside -- most "write" actions will be disabled
        #   => you always need to know the span of items present in viewport to be rendered in O(1)
        c_item = C.color_pair(ColorMap.default)
        c_auxinfo = C.color_pair(ColorMap.auxinfo)
        c_cursor = C.A_REVERSE | C.A_BOLD  # OR: C.color_pair(ColorMap.cursor)

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
            nm, _, aux = item.name.partition("\n")
            stdscr.addstr(y, 3, nm, c_item)
            for i, x in enumerate(aux.split("\n")):
                stdscr.addstr(y + 1 + i, 5, x, c_auxinfo)

        cursor_y = self._viewport.above
        nm, _, aux = self.focused_item.name.partition("\n")
        stdscr.addstr(cursor_y, 3, nm, c_cursor)
        for i, x in enumerate(aux.split("\n")):
            stdscr.addstr(cursor_y + 1 + i, 5, x, c_cursor)

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
            nm, _, aux = item.name.partition("\n")
            stdscr.addstr(y, 3, nm, c_item)
            for i, x in enumerate(aux.split("\n")):
                stdscr.addstr(y + 1 + i, 5, x, c_auxinfo)
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
    _wdg: NaviWidget

    def set_entity(self, ent: Representable) -> None:
        self._ent = ent
        if (sfn := getattr(ent, "explore")) and callable(sfn):
            self._act = sfn  # NOTE: keep sfn to be able to refresh() the list (when externally changed)
            self._lst = self._act()
            # TEMP:TRY:
            self._wdg = NaviWidget()
            self._wdg.assign(self._lst)

    def cursor_move_rel(self, modifier: int) -> None:
        self._wdg.step_by(modifier)

    def resize(self, stdscr: C.window) -> None:
        self._wdg.resize(*stdscr.getmaxyx())
        self.redraw(stdscr)

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)
        # FIXED: prevent crash when window shrinks past the cursor
        # self.cursor_move_rel(0)
        # NOTE: actually _lst here stands for a generic _augdbpxy with read.API
        #   i.e. DB augmented by virtual entries, all generated-and-cleared on demand
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
    from .widget import RootWidget  # pylint:disable=import-self,redefined-outer-name

    g.root_wdg = wdg = RootWidget()
    # wdg.set_entity(FSEntry("/etc/udev"))
    wdg.set_entity(FSEntry("/d/airy"))
    g.curses_ui.resize()
    # wdg.redraw(g.stdscr)
    # g.stdscr.refresh()
