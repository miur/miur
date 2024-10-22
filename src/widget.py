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
        self._viewport_origin_yx = (0, 0)
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

    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        # pvh = self._viewport_height_lines
        self._viewport_origin_yx = origin
        self._viewport_height_lines = vh
        self._viewport_width_columns = vw
        self._viewport_margin_lines = vh // 6  # OR: fixed=2
        # KEEP: self._viewport_followeditem_lstindex
        # TODO: adjust resulting offset to align onto margin
        # if pvh > 0 and (ratio := self._viewport_followeditem_linesfromtop / pvh) > 0:
        #     self._viewport_followeditem_linesfromtop = int(vh * ratio)
        self._viewport_followeditem_linesfromtop = 0

    # CASE:(lightweight): to be able to re-assign ~same list after external xfm, e.g. after "order-by"
    def assign(self, lst: Sequence[Representable]) -> None:
        pidx = self._cursor_item_lstindex
        focused = self._lst[pidx] if getattr(self, "_lst", None) else None
        # WARN: whole function should be atomic
        #   i.e. "cursor,canvas" should always be in boundaries of "lst"
        # TODO: pre-load only visible part fitting into viewport
        #   WARN: on first assign(), viewport height may still be =0, due to -resize() being called later
        self._lst = lst
        # TODO: if item under cursor had disappeared we can temp-reinsert the _focused_item into the list
        #   and seek for it to find a new index, then pick item before or after expected position
        # NOTE: search for the item if "order-by" have changed its index
        ## TEMP: reset *cursor* position on -assign()
        if focused is not None and focused is not lst[pidx]:
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
    # TODO:OPT: always step by whole item ~~ should be the same as "step_by(+/-inf)"
    def step_by(self, steps: int) -> None:
        if steps not in (-1, 1):
            raise NotImplementedError("DECI:WiP")

        idx = self._cursor_item_lstindex
        # newidx = idx + steps
        # self._cursor_item_lstindex = newidx
        # self._viewport_followeditem_lstindex = newidx

        # rng = range(min(idx, newidx), max(idx, newidx))
        # hlines = sum(self._itemheight(self._lst[i]) for i in rng)
        # RENAME: delta/advance/shift
        # offset = -hlines if steps < 0 else hlines
        # TEMP:FAIL: can't scroll, list is limited to the size of viewport
        # TODO: offset=0 if linesfromtop < margin or linesfromtop > vh - margin
        # self._viewport_followeditem_linesfromtop += offset

        vh = self._viewport_height_lines
        margin = self._viewport_margin_lines
        pos = self._viewport_followeditem_linesfromtop
        knock = 0
        # ALT:IDEA:(step_incr=steps): only allow for "step_by(arg)" to move by one item/index,
        #   and use "arg" to pick speed of scrolling multiline items instead
        step_incr = 2  # RENAME? single_step/step_advance
        advance = int(step_incr * steps)  # OR: math.round(abs())

        # pylint:disable=no-else-raise
        # ARCH:FSM:(x4): sign/idx/vp/size
        if steps > 0:

            last = len(self._lst) - 1

            if idx < 0 or idx > last:
                raise IndexError(idx)
            elif idx < last:

                if pos >= vh:
                    raise NotImplementedError(
                        "TEMP: restricted; sync lost: vp had drifted above cursor"
                    )
                elif pos < 0:
                    if pos <= -ih:
                        raise NotImplementedError(
                            "TEMP: restricted; sync lost: vp had drifted below cursor"
                        )
                    else:
                        raise NotImplementedError(
                            "TEMP: restricted; only *last* large multine item can start above vp"
                        )
                elif pos < vh - margin - 1:
                    raise NotImplementedError(
                        "TBD: normal ops; move both index and pos"
                    )

                    ## FIXME: scroll small items vh(item)<4 whole by each step, and large ones by step_incr,
                    ##   transferring advancement residue onto next item
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

                    ## WARN: if steps>1 and will cross margin -- we should apply block from here,
                    ##   and then next block from margin
                    # newidx = idx + steps
                    # self._cursor_item_lstindex = newidx
                    # self._viewport_followeditem_lstindex = newidx
                    # pos += advance
                elif pos < vh:
                    raise NotImplementedError(
                        "TBD: margin ops; keep pos until we at the end of the list"
                    )
                else:
                    raise ValueError("unexpected")

            elif idx == last:
                ih = self._itemheight(self._lst[idx])
                if pos <= -ih:
                    # ALT: allow gradual alignment, like in {hidden_part < 0}
                    raise NotImplementedError(
                        "last item is far above vp, nothing on the screen"
                    )
                elif pos >= vh:
                    # ALT: scroll drifted vp until item becomes visible -> then show item hidden_part as usual
                    # OR: imm jump vp to the current cursor position *and* move cursor by one step as usual
                    # OR: jump cursor to current drifted vp top/bot focuseditem based on if <j/k> was pressed
                    raise NotImplementedError(
                        "currently restricted; last item first line should be visible"
                    )
                elif (
                    pos < vh - ih
                ):  # NOTE:(ih>vh is OK): multiline items can be larger than viewport
                    # CHECK: already implemented gradual alignment -- will it work or not? CHECK: is it intuitive ?
                    # [_] BAD:FIXME: should do nothing for short lists {sum(vh(i))<vh}
                    raise NotImplementedError(
                        "currently restricted; last item bot shouldn't be above vp bot"
                    )
                else:
                    ## NOTE: scroll canvas/viewport if cursor is on first/last item, but it's only partially shown
                    # RENAME? visible_{part,below,range,room}/preview_{span,window}
                    visible_room = vh - pos
                    hidden_part = ih - visible_room
                    if hidden_part > 0:
                        # NOTE:(zoom-out): move vp away to make room for the last item to be visible on screen
                        #   OK: "pos" may become large negative to fit bot part of large multiline item into vp
                        if advance > hidden_part:
                            knock = advance - hidden_part
                            pos -= hidden_part
                        else:
                            pos -= advance
                    elif hidden_part == 0:
                        # NOTE: detect the attempt to cross the border (bot of last multiline item)
                        # TODO: visual "knock()" for 100ms when attempting to scroll past top/bot item
                        knock = advance
                    elif hidden_part < 0:
                        # NOTE: gradually align bot of last item with bot of vp
                        #   ~~ may happen if vp was centered around last item first
                        # ALT:OPT: allow scrolling till only last line of last item visible
                        #   i.e. {pos -= advance} until {ih + pos == 1}
                        empty_space = -hidden_part
                        if advance > empty_space:
                            knock = advance - empty_space
                            pos += empty_space
                        else:
                            pos += advance

        elif steps < 0:
            raise NotImplementedError()

            # elif sum(f_h(0 .. newidx)) < margin:
            #     raise NotImplementedError()
            # elif sum(f_h(newidx .. len(self._lst))) < margin:
            #     raise NotImplementedError()

            if pos < 0:
                raise NotImplementedError("past last multiline item")
            elif pos == 0:
                raise NotImplementedError()
            elif pos < margin:
                raise NotImplementedError()
            elif pos < vh - margin - 1:
                raise NotImplementedError()
            elif pos < vh - 1:
                raise NotImplementedError()
            elif pos == vh - 1:
                raise NotImplementedError()
            elif pos >= vh:
                raise NotImplementedError()

    # def scroll_by(self, advance: int) -> None:

    def redraw(self, stdscr: C.window) -> None:
        # draw_footer(stdscr)
        # ARCH:WARN: we actually need to render whatever is *shown in viewport* (even if cursor is far outside)
        #   COS: when cursor is outside -- most "write" actions will be disabled
        #   => you always need to know the span of items present in viewport to be rendered in O(1)
        c_item = C.color_pair(ColorMap.default)
        c_auxinfo = C.color_pair(ColorMap.auxinfo)
        c_iteminfo = C.color_pair(ColorMap.iteminfo)
        c_pfxrel = c_auxinfo
        c_pfxidx = c_iteminfo
        c_cursor = C.A_REVERSE | C.A_BOLD  # OR: C.color_pair(ColorMap.cursor)

        # log.verbose(f"list: [<={vp.h}/{len(lst)}]")

        # self._viewport_margin_lines
        ci = self._cursor_item_lstindex
        vh = self._viewport_height_lines
        vw = self._viewport_width_columns
        top_idx = self._viewport_followeditem_lstindex
        # WARN! we assume that: { top of NaviWidget = top of RootWidget = 0,0 }
        top_y = self._viewport_followeditem_linesfromtop
        while top_idx > 0 and top_y > 0:
            top_idx -= 1
            top_y -= self._itemheight(self._lst[top_idx])
        # log.trace(f"{top_y} {top_idx=}")

        last = len(self._lst) - 1
        i, y = top_idx, top_y
        vy, vx = self._viewport_origin_yx
        while i <= last and y < vh:
            item = self._lst[i]
            rel = i - top_idx
            pfxrel = f"{1+rel:02d}| "
            pfxidx = f"{1+i:03d}{">" if i == ci else ":"} "
            indent = len(pfxrel) + len(pfxidx)
            nm, *lines = item.name.split("\n")
            py = y
            if 0 <= y < vh:
                stdscr.addstr(vy + y, vx + 0, pfxrel, c_pfxrel)
                stdscr.addstr(
                    vy + y, vx + len(pfxrel), pfxidx, c_cursor if i == ci else c_pfxidx
                )
                stdscr.addstr(
                    vy + y,
                    vx + indent,
                    nm[: vw - indent],
                    c_cursor if i == ci else c_item,
                )
            y += 1
            for l in lines:
                if 0 <= y < vh:
                    # stdscr.addstr(vy+y, 2, "|", c_pfxrel)
                    stdscr.addstr(
                        vy + y,
                        vx + indent + 2,
                        l[: vw - indent - 2],
                        c_cursor if i == ci else c_iteminfo,
                    )
                y += 1
            if y - py != self._itemheight(item):
                log.error(f"{y - py} != {self._itemheight(item)}")
                raise RuntimeError("WTF: this exception is silently ignored")
            i += 1

        ## ALT:NOTE: draw cursor AGAIN after footer (i.e. over-draw on top of full list)
        ##   NICE: no need to hassle with storing cursor prefix length for cx/cy
        ##   NICE: can redraw only two lines (prev item and cursor) inof whole list
        # cx = len(_pfx(vctx.wndcurpos0))
        # _draw_item_at(vctx.wndcurpos0, citem)
        # stdscr.move(vctx.wndcurpos0, cx)
        ## OR:BET: only change attributes of already printed line
        # cx = len(_pfx(vctx.wndcurpos0))
        # cn = len(lst[vctx.wndcurpos0 + vctx.wndabsoff0].name)
        # stdscr.chgat(vctx.wndcurpos0, cx, cn, ccurs)


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
    _wh: int
    _ww: int

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
        self._wh, self._ww = stdscr.getmaxyx()
        self._wdg.resize(self._wh - 2, self._ww, origin=(1, 0))
        self.redraw(stdscr)

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)
        # FIXED: prevent crash when window shrinks past the cursor
        # self.cursor_move_rel(0)
        # NOTE: actually _lst here stands for a generic _augdbpxy with read.API
        #   i.e. DB augmented by virtual entries, all generated-and-cleared on demand
        self._wdg.redraw(stdscr)

        # pylint:disable=protected-access
        c_footer = C.color_pair(ColorMap.footer)
        ci = 1 + self._wdg._cursor_item_lstindex
        sz = len(self._wdg._lst)
        sortby = "name"
        sortrev = False
        footer = f"--- {ci}/{sz} | by={sortby}{"￪" if sortrev else "￬"}"
        stdscr.addstr(self._wh - 1, 0, footer, c_footer)

        if isinstance(self._ent, FSEntry):
            header = str(self._ent._x)
        else:
            header = repr(self._ent)
        stdscr.addstr(0, 0, header, c_footer | C.A_BOLD)

        # NOTE: place real cursor to where list-cursor is, to make tmux overlay selection more intuitive
        cy = self._wdg._viewport_origin_yx[0]
        cx = self._wdg._viewport_origin_yx[1] + 4  # = len(pfx)
        pos = self._wdg._viewport_followeditem_linesfromtop
        if 0 <= pos < self._wdg._viewport_height_lines:
            cy += pos
        stdscr.move(cy, cx)

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
