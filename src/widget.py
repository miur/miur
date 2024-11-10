import os
from typing import Callable, Iterable, Protocol, Sequence, override

import _curses as C

from . import widget as this  # pylint:disable=import-self
from .curses_ext import ColorMap
from .util.logger import log


class Representable(Protocol):
    @property
    def name(self) -> str: ...


# RENAME? my algo is "SatelliteViewport"
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
        self._viewport_margin_lines = vh // 8  # OR: fixed=2
        # KEEP: self._viewport_followeditem_lstindex
        # TODO: adjust resulting offset to align onto margin
        # if pvh > 0 and (ratio := self._viewport_followeditem_linesfromtop / pvh) > 0:
        #     self._viewport_followeditem_linesfromtop = int(vh * ratio)
        self._viewport_followeditem_linesfromtop = 0

    # CASE:(lightweight): to be able to re-assign ~same list after external xfm, e.g. after "order-by"
    def assign(self, lst: Sequence[Representable], hint_idx: int | None = None) -> None:
        pidx = self._cursor_item_lstindex if hint_idx is None else hint_idx
        focused = self._lst[pidx] if getattr(self, "_lst", None) else None
        # WARN: whole function should be atomic
        #   i.e. "cursor,canvas" should always be in boundaries of "lst"
        # TODO: pre-load only visible part fitting into viewport
        #   WARN: on first assign(), viewport height may still be =0, due to -resize() being called later
        self._lst = lst
        newidx = self._reindex(pidx, focused)
        self._viewport_followeditem_lstindex = self._cursor_item_lstindex = newidx
        # KEEP: self._viewport_followeditem_linesfromtop

    def _reindex(self, pidx: int, focused: Representable) -> int:
        # NOTE: keep cursor on same item::
        #   * if any items were inserted/deleted before idx
        #   * if "order-by" have changed its item's idx in _lst
        if focused is None:
            return 0
        if pidx < len(self._lst) and focused is self._lst[pidx]:
            return pidx
        try:
            return self._lst.index(focused)
        except ValueError:
            ## TEMP: reset *cursor* position on .assign(newlst)
            # TODO: if item under cursor had disappeared we can temp-reinsert the _focused_item into the list
            #   and seek for it to find a new index, then pick item before or after expected position
            return 0

    # MAYBE: make ItemWidget to calc() item height and draw it (or only ItemXfm)
    #   WARN: don't store "index" inside ItemWidget << PERF:(slow): rebuild on each order-by
    #     ~~ though, I can pass list index ctx into ItemWidget.render(ctx[index=i])
    #   NICE:IDEA: make scrollbar ~imprecise/elastic~ and scroll through *items* when jumping far,
    #     but scroll by lines when scrolling around current viewport, where we already know all items heights
    @staticmethod
    def _itemheight(item: Representable) -> int:
        return item.name.count("\n") + 1

    # TBD: def scroll_by(self, advance: int) -> None:

    # NOTE: elastic scroll, where "step" is anything between 1 line/word or whole multiline item,
    #   depending on what less disrupts the perception flow
    # TODO:OPT: always step by whole item ~~ should be the same as "step_by(+/-inf)"
    def step_by(self, steps: int) -> None:
        if steps not in (-1, 1):
            raise NotImplementedError("DECI:WiP")

        def _fih(i: int) -> int:
            return self._itemheight(self._lst[i])

        idx = self._cursor_item_lstindex
        last = len(self._lst) - 1
        if idx < 0 or idx > last:
            raise IndexError(idx)
        ih = _fih(idx)

        # rng = range(min(idx, newidx), max(idx, newidx))
        # hlines = sum(self._itemheight(self._lst[i]) for i in rng)
        # RENAME: delta/advance/shift
        # offset = -hlines if steps < 0 else hlines
        # TEMP:FAIL: can't scroll, list is limited to the size of viewport
        # TODO: offset=0 if linesfromtop < margin or linesfromtop > vh - margin
        # self._viewport_followeditem_linesfromtop += offset

        vh = self._viewport_height_lines
        bot = vh - 1
        margin = self._viewport_margin_lines
        if vh < 2 * margin + 1:
            raise NotImplementedError(
                "TEMP:(restricted): viewport with enough space inside"
            )
        # NOTE:(pos<0|ih>vh is OK): multiline items can be larger than viewport
        pos = self._viewport_followeditem_linesfromtop

        knock = 0  # <EXPL: amount of unused "steps" (for either switching or scrolling)
        # ALT:IDEA:(step_incr=steps): only allow for "step_by(arg)" to move by one item/index,
        #   and use "arg" to pick speed of scrolling multiline items instead
        step_incr = 2  # <TEMP|RENAME? single_step/step_advance
        advance = int(step_incr * steps)  # OR: math.round(abs())

        # IDEA: visualize all these "margin" and sumheight/etc. to get a visual feedback
        #   from all boundary conditions -- and prevent errors like "off by 1"
        #   ~~ only visualize upp/bot "margin" based on last "steps" direction (i.e. dynamically)

        # IDEA: visualize scroll-animation when scrolling long items at once
        #   << orse they "jump into the face" when you are scrolling around margin

        # RQ: last item can be above bot only if list is shorter than vp
        #   ALT:CHECK:(gravitate): "gradually align of last item to bot" -- will it be *intuitive* ?
        if (
            idx == last
            and pos + ih < bot
            and (last >= bot or sum(_fih(i) for i in range(0, last + 1)) >= bot)
        ):
            raise NotImplementedError(
                "RQ: last item can be above bot only if list is shorter than vp"
            )

        # ARCH:FUT: it's a FSM(sign/idx/vp/size) -- should we make it explicit?
        # pylint:disable=chained-comparison,no-else-raise
        if pos < 0 or pos > bot:
            raise NotImplementedError("TEMP:(restricted): cursor should be visible")
            # raise NotImplementedError("past last multiline item")
            # if steps > 0 and idx < last:
            #     if pos > bot:
            #         raise NotImplementedError("TEMP: restricted; sync lost: vp had drifted above cursor")
            #     elif pos < 0:
            #         if pos <= -ih:
            #             raise NotImplementedError("TEMP: restricted; sync lost: vp had drifted below cursor")
            #         else:
            #             # THINK: actually *any* current item can be larger than vp
            #             raise NotImplementedError("TEMP: restricted; only *last* large multine item can start above vp")
            # elif idx == last:
            #     if pos <= -ih:
            #         # ALT: allow gradual alignment, like in {hidden_part < 0}
            #         raise NotImplementedError(
            #             "TEMP: restricted; last item is far above vp, nothing on the screen"
            #         )
            #     elif pos > bot:
            #         # ALT: scroll drifted vp until item becomes visible -> then show item hidden_part as usual
            #         # OR: imm jump vp to the current cursor position *and* move cursor by one step as usual
            #         # OR: jump cursor to current drifted vp top/bot focuseditem based on if <j/k> was pressed
            #         raise NotImplementedError(
            #             "TEMP: restricted; last item first line should be visible"
            #         )
        elif pos == 0 and steps < 0:
            idx += steps
            if idx < 0:
                knock = idx
                idx = 0
        elif pos == bot and steps > 0:
            # NOTE: preserve pos until the last item in list
            #   OR? auto-jump (or gravitate) to either margin or bot, based on how much lst is left
            idx += steps
            if idx > last:
                knock = idx - last
                idx = last
        elif pos <= margin and steps < 0:
            idx += steps
            if idx < 0:
                knock = idx
                idx = 0
            if pos > idx:  # <PERF:HACK
                # NOTE: if some item ih=0, then enclosing perf hack will leave gaps till first item
                assert all(_fih(i) >= 1 for i in range(0, idx))
                lines_top = sum(_fih(i) for i in range(0, idx))
                if lines_top < margin:
                    pos = lines_top
        elif pos >= bot - margin and steps > 0:
            ## FAIL: there is no way to detect when to switch to next item,
            #   as "virtual offset inside item" is derived from dynamic "offset from top"
            # MAYBE: we still need a "vp-virtual cursor" to point to "subparts" of the items
            #   ARCH: virt.cursor coincides with user's attention focus and limited by vp boundaries
            #     >> so "lst-cursor" is simply one of possible projections of virt.cursor onto lst
            #       i.e. multiple virt positions match single lst item
            #       HACK:(vice-versa): in zoom-out preview of the list one virt.cursor
            #         may match multiple lst items, which you can expand or zoom-in
            #   NICE? we can make {pos>=0} always positive, and control top item offset by subcursor
            #   IDEA: use 2D(y,x) subcursor to impl a wf-like "row of buttons" under each item
            #     (or to navigate individual words of multiline item/text)

            ## FIXME: {advance<ih} -> {virt.cursor-cursor+advance > ih}
            # if advance < ih:
            #     raise NotImplementedError(
            #         "TBD: scroll large item by steps, keeping virt.pos the same\n" + str(locals())
            #     )

            ## NOTE: jump to next item {if current one is small} and discard residue,
            # [_] OR:TODO: scroll large item by "steps"-proportional advancement
            # [_] ALSO:WF:TRY?(irritating?): transfer advancement residue onto next item scroll
            #   ~~ can be either more intuitive OR more disrupting
            idx += steps
            if idx > last:
                remainder = idx - last
                advance = int(remainder * step_incr)
                idx = last

            if idx == last:
                ## NOTE: scroll partially shown last item
                # BAD! should also apply to above branch {pos==bot}
                #   ~~ BUT: may occur only if "RND:(invariant): cursor should be on margin" was broken
                # RENAME? visible_{part,below,range,room}/preview_{span,window}
                visible_room = vh - pos
                hidden_part = ih - visible_room
                if hidden_part > 0:
                    # NOTE:(zoom-out): move vp away to make room for the last item to be visible on screen
                    if advance > hidden_part:
                        knock, residue = divmod(advance - hidden_part, step_incr)
                        if residue > 0:
                            knock += 1
                        pos -= hidden_part
                    else:
                        # OK: "pos" may become large negative to fit bot part of large multiline item into vp
                        pos -= advance
                elif hidden_part == 0:
                    # NOTE: detect the attempt to cross the border (bot of last multiline item)
                    # TODO:(animation): visual "knock()" for 100ms when attempting to scroll past top/bot item
                    knock, residue = divmod(advance - hidden_part, step_incr)
                    if residue > 0:
                        knock += 1
                elif hidden_part < 0:
                    # NOTE:(gravitate): gradually align bot of last item with bot of vp
                    #   ~~ may happen if vp was centered around last item first
                    # ALT:OPT: allow scrolling till only last line of last item visible
                    #   i.e. {pos -= advance} until {ih + pos == 1}
                    empty_space = -hidden_part
                    if advance > empty_space:
                        knock, residue = divmod(advance - empty_space, step_incr)
                        if residue > 0:
                            knock += 1
                        pos += empty_space
                    else:
                        pos += advance
                        assert pos + ih <= bot
                else:
                    raise ValueError("unexpected")

            if bot - pos > last - idx:  # <PERF:HACK
                assert all(_fih(i) >= 1 for i in range(idx, last + 1))
                ## NOTE: gravitate to bot
                # ALT:NOT? calc from bot-up until found how much items fit under margin
                #   ~~ for i in range(last, max(0,last-margin)); do if ... break; bot_edge_idx = i;
                lines_bot = sum(_fih(i) for i in range(idx, last + 1)) - 1
                if lines_bot < margin:
                    # NOTE: immediately show last {small} items fully
                    pos = bot - lines_bot
                    assert 0 <= pos <= bot
                elif pos != bot - margin:
                    # ALT:MAYBE? gravitate cursor back to margin
                    raise RuntimeWarning("RND:(invariant): cursor should be on margin")
            elif pos != bot - margin:
                # ALT:MAYBE? gravitate cursor back to margin
                raise RuntimeWarning("RND:(invariant): cursor should be on margin")

        elif steps < 0:
            pidx = idx
            idx += steps
            if idx < 0:
                knock = idx
                idx = 0
            lines_step_up = sum(_fih(i) for i in range(idx, pidx))
            pos -= lines_step_up
            # NOTE: keep at margin, or step over it, or adhere to the end
            if pos <= margin:
                pos = margin
                if idx <= margin:
                    assert all(_fih(i) >= 1 for i in range(0, idx))
                    lines_top = sum(_fih(i) for i in range(0, idx))
                    if lines_top <= margin:
                        pos = lines_top
        elif steps > 0:
            ## NOTE: always jump to next item (whatever item size)
            # TODO: scroll by "steps" if item is larger than e.g. half-viewport
            pidx = idx
            idx += steps
            if idx > last:
                knock = idx - last
                idx = last
            # FUT: allow advancing by partial items
            lines_step_down = sum(_fih(i) for i in range(pidx, idx))
            pos += lines_step_down
            if pos >= bot - margin:
                pos = bot - margin
                if bot - pos >= last - idx:
                    assert all(_fih(i) >= 1 for i in range(idx, last + 1))
                    lines_bot = sum(_fih(i) for i in range(idx, last + 1)) - 1
                    if lines_bot <= margin:
                        pos = bot - lines_bot
        else:
            raise ValueError("unexpected")

        # FUT: don't assign if the same
        self._cursor_item_lstindex = idx
        self._viewport_followeditem_lstindex = idx
        self._viewport_followeditem_linesfromtop = pos

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
        # return str(self._x)
        # TEMP:TEST: multiline entries
        return str(self._x).replace("o", "o⬎\n")

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

    def set_entity(self, ent: Representable, hint_idx: int | None = None) -> None:
        self._ent = ent
        if (sfn := getattr(ent, "explore")) and callable(sfn):
            self._act = sfn  # NOTE: keep sfn to be able to refresh() the list (when externally changed)
            self._lst = self._act()
            # TEMP:TRY:
            self._wdg = NaviWidget()
            self._wdg.assign(self._lst, hint_idx)

    def cursor_move_rel(self, modifier: int) -> None:
        self._wdg.step_by(modifier)

    def resize(self, stdscr: C.window) -> None:
        self._wh, self._ww = stdscr.getmaxyx()
        self._wdg.resize(self._wh - 2, self._ww, origin=(1, 1))
        self.redraw(stdscr)

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)
        # FUT: only clear "rest of each line" -- and only if prev line there was longer
        stdscr.clear()
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

        # HACK:(this): force cmp with new instance after reload()
        if isinstance(self._ent, this.FSEntry):
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
    from .util.exchook import log_exc
    from .widget import RootWidget  # pylint:disable=import-self,redefined-outer-name

    hidx = None
    if pwdg := getattr(g.root_wdg, "_wdg", None):
        hidx = pwdg._cursor_item_lstindex  # pylint:disable=protected-access

    i: int
    try:
        g.root_wdg = wdg = RootWidget()
        # wdg.set_entity(this.FSEntry("/etc/udev"), hint_idx=hidx)
        wdg.set_entity(this.FSEntry("/d/airy"), hint_idx=hidx)
        g.curses_ui.resize()

        # ALT: fuzzy-test, by random direction of up/down 50 times
        ndown = 30
        for i in range(ndown):
            wdg.cursor_move_rel(1)
        wdg.redraw(g.stdscr)
        g.stdscr.refresh()
        for i in range(ndown):
            wdg.cursor_move_rel(-1)
        wdg.redraw(g.stdscr)
        g.stdscr.refresh()
    except Exception as exc:  # pylint:disable=broad-exception-caught
        exc.add_note(f"{i=}")
        log_exc(exc)
