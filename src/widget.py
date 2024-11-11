import os
import os.path as fs
from typing import Callable, Iterable, Protocol, Self, Sequence, override

import _curses as C

from . import widget as this  # pylint:disable=import-self
from .curses_ext import ColorMap
from .util.logger import log


class Representable(Protocol):
    @property
    def name(self) -> str: ...

    # MOVE? "Sortable" ?
    def __lt__(self, other: Self) -> bool:
        return self.name < other.name


class Addressable(Protocol):
    @property
    def loci(self) -> str: ...


class Explorable(Protocol):
    # BET? def explore(self) -> Iterable[Representable] | HaltEntry/None: ...
    #   ALT: allow .explore to raise NotImplementedError() as regular *frequent* behavior
    #     BAD: we need a way to query "if Explorable" to indicate it in `Viewport before opening
    def explore(self) -> Iterable[Representable]: ...


class Golden(Representable, Addressable, Explorable, Protocol):
    __slots__ = ()


class SatelliteViewport:  # pylint:disable=too-many-instance-attributes
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
        # MAYBE:XLR: point cursor to folder/_ent itself
        #   + makes cursor always deterministic
        #   + allows you the subset of operations, like adding new files to the folder
        if not self._lst:
            # BET? return dummy placeholder for empty dirs
            #   BAD! placeholder is *content*, it shouldn't be *focused* either
            #   ALT: always include dir itself in listing -- so we could do ops from inside the dir
            raise IndexError("empty list")
        return self._lst[self._cursor_item_lstindex]

    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        pvh = self._viewport_height_lines
        self._viewport_origin_yx = origin
        self._viewport_height_lines = vh
        self._viewport_width_columns = vw
        self._viewport_margin_lines = vh // 8  # OR: fixed=2
        # KEEP: self._viewport_followeditem_lstindex
        # RND: adjust resulting offset to align onto margin
        #   ALT:BAD: self._viewport_followeditem_linesfromtop = 0
        if pvh > 0 and (ratio := self._viewport_followeditem_linesfromtop / pvh) > 0:
            self._viewport_followeditem_linesfromtop = int(vh * ratio)

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
        ## DISABLED: we always keep the previous position of cursor on the screen
        #   self._viewport_followeditem_linesfromtop = 0

    def _reindex(self, pidx: int, focused: Representable | None) -> int:
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

        if not self._lst:
            log.trace("EMPTY")
            return

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
                # [_] WTF:ERR:CASE: open "/" and scroll down -- it gravitates :(
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
        c_error = C.color_pair(ColorMap.error)
        c_fsdir = C.color_pair(ColorMap.fsdir)
        c_fslink = C.color_pair(ColorMap.fslink)

        vy, vx = self._viewport_origin_yx
        if not self._lst:
            stdscr.addstr(vy, vx, "[ EMPTY ]", c_error | c_cursor)
            return

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
        while i <= last and y < vh:
            item = self._lst[i]
            if isinstance(item, HaltEntry):
                stdscr.addstr(vy + y, vx + 0, f"[ {item.name} ]", c_error | c_cursor)
                i += 1
                return

            rel = i - top_idx
            pfxrel = f"{1+rel:02d}| "
            # TODO: for binary/hex show "file offset in hex" inof "item idx in _xfm_list"
            pfxidx = f"{1+i:03d}{">" if i == ci else ":"} "
            indent = len(pfxrel) + len(pfxidx)
            nm, *lines = item.name.split("\n")
            py = y
            if 0 <= y < vh:
                stdscr.addstr(vy + y, vx + 0, pfxrel, c_pfxrel)
                stdscr.addstr(
                    vy + y, vx + len(pfxrel), pfxidx, c_cursor if i == ci else c_pfxidx
                )
                xoff = vx + indent
                if not isinstance(item, FSEntry):
                    c_schema = c_item
                elif fs.islink(item.loci):
                    c_schema = c_fslink
                    if fs.isdir(item.loci):
                        c_schema |= C.A_BOLD
                elif fs.isdir(item.loci):
                    c_schema = c_fsdir
                else:
                    c_schema = c_item
                stdscr.addstr(
                    vy + y,
                    xoff,
                    nm[: vw - xoff],
                    c_schema | c_cursor if i == ci else c_schema,
                )
            y += 1
            for l in lines:
                if 0 <= y < vh:
                    # stdscr.addstr(vy+y, 2, "|", c_pfxrel)
                    xoff = vx + indent + 2
                    stdscr.addstr(
                        vy + y,
                        xoff,
                        l[: vw - xoff],
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


# RENAME? ErrorEntry -- BUT: such entry should also be explorable...
class HaltEntry(Golden):
    def __init__(self, kind: str, loci: tuple[str, ...] | None = None) -> None:
        self._msg = kind
        self._orig = loci

    @override
    @property
    def name(self) -> str:
        return self._msg

    @override
    @property
    def loci(self) -> str:
        return "".join(self._orig) if self._orig else "∅ " + repr(self)

    @override
    def explore(self) -> Iterable[Representable]:
        raise TypeError(self._msg)


class TextEntry(Golden):
    __slots__ = ()  # "name", "loci", "explore")

    def __init__(self, text: str, loci: tuple[str, ...] | None = None) -> None:
        self._x = text
        self._at = loci  # = (path,lnum,[col,[boff,...]])

    @override
    @property
    def name(self) -> str:
        return self._x

    @override
    @property
    def loci(self) -> str:
        return "".join(self._at) if self._at else "∅ " + repr(self)

    @override
    def explore(self) -> Iterable[Representable]:
        from re import finditer

        cls = type(self)
        words = [
            cls(
                m.group(0),
                loci=(
                    (
                        self._at[0],
                        f":{m.start()}+{m.end() - m.start()}",
                        self._at[-1],
                    )
                    if self._at
                    else ("∅", f":{m.start()}+{m.end() - m.start()}")
                ),
            )
            for m in finditer(r"\S+", self._x)
        ]
        if len(words) <= 1:
            return [HaltEntry("ATOMIC", loci=self._at)]  # ALT: "NOT EXPLORABLE (YET)"
        return words


class FSEntry(Golden):
    __slots__ = ()  # "name", "loci", "explore")

    def __init__(self, path: str, nm:bool|str = True, alt: bool = False) -> None:
        self._x = path
        self._nm = fs.basename(path) if nm is True else path if nm is False else nm
        self._alt = alt

    # NICE: as we have a separate .name field, we can augment regular filenames
    # ex~:
    #   ~ skip date-prefix (to sort by body)
    #   ~ substitute my xts3 to isodate
    #   ~ insert full path into name -- for flattened folders
    @override
    @property
    def name(self) -> str:
        # return str(self._x)
        # TEMP:TEST: multiline entries
        return self._nm.replace("o", "o⬎\n")

    @override
    @property
    def loci(self) -> str:
        return self._x

    # i.e. =InterpretUnchangedDirListingPropertyAsFSEntriesInUsualWay
    @override
    def explore(self) -> Iterable[Representable]:
        p = self._x
        if not fs.lexists(p):
            return [HaltEntry("FILE NOT FOUND", loci=(self._x,))]
        if not fs.exists(p):
            return [HaltEntry("DANGLING SYMLINK", loci=(fs.realpath(self._x),))]
        cls = type(self)
        # [_] TRY: print this info on 2nd/3rd line below the link, as auxinfo
        #   BAD: we are losing unified access to copy/edit/navi the interpreted symlink values as regular items
        #     ALT:NICE: with subcursor we could navi/copy even those 2nd/3rd lines of info
        # [?] ALT:FIXME: skip .islink if .originator==self
        #   NICE: we can walk up through .originator chain and construct full "loci" for pieces
        #   ALT: produce virtual FSEntryLike entry, as it's a collection of paths inof real folder
        #     NICE: preserves original order inof being sorted by default as all other FSEntry
        if not self._alt and fs.islink(p):
            return [cls(p, nm=False, alt=True), cls(os.readlink(p), nm=False), cls(fs.realpath(p), nm=False), cls(fs.relpath(fs.realpath(p), p), nm=False)]
        if fs.isdir(p):
            with os.scandir(p) as it:
                return [cls(e.path) for e in it]
        if fs.isfile(p):
            try:
                textlines: list[TextEntry] = []
                with open(p, "r", encoding="utf-8") as f:
                    # ALT:(python>=3.13): lines = f.readlines(sizehint=1024, keepends=False)
                    i = 1
                    while (boff := f.tell()) < 1024 and (line := f.readline(1024)):
                        ent = this.TextEntry(
                            line.removesuffix("\n"), loci=(p, f":{i}", f"  `{boff}")
                        )
                        textlines.append(ent)
                        i += 1
                return textlines
            except UnicodeDecodeError:
                # TODO: on redraw() show "file offset in hex" inof "item idx in _xfm_list"
                hexlines: list[TextEntry] = []
                with open(p, "rb") as f:
                    i = 1
                    while (boff := f.tell()) < 1024 and (data := f.read(16)):
                        ent = this.TextEntry(
                            data.hex(" "), loci=(p, f" `0x{boff:x}  #{i}")
                        )
                        hexlines.append(ent)
                        i += 1
                return hexlines
        raise NotImplementedError(p)


# T = TypeVar("T")
# class ListCachingProxy(list[T]):
#     pass


# ALT:SPLIT: make an `EntityContext for serialization/restoration on restart
class EntityView:
    _ent: Representable
    _originator: Self | None
    _wdg: SatelliteViewport
    _act: Callable[[], Sequence[Representable]]
    # _lstpxy: ListCachingProxy[Representable]
    _orig_lst: Sequence[Representable]
    _xfm_lst: list[Representable]

    # ALT:BAD?PERF: store in each TextEntity a backref to "originator"
    #   == to be able to return to its "parent"
    #   NICE: only "navigated-to" items will store this backref
    def __init__(self, ent: Representable, originator: Self | None = None) -> None:
        assert not isinstance(ent, HaltEntry)
        self._ent = ent
        # NOTE: remember which `View have created this one -- tba to return back
        self._originator = originator
        # ALT:PERF(slow): @runtime_checkable : isinstance(ent, Explorable)
        #   https://mypy.readthedocs.io/en/latest/protocols.html#using-isinstance-with-protocols
        if (sfn := getattr(ent, "explore", None)) and callable(sfn):
            self._act = sfn  # NOTE: keep sfn to be able to refresh() the list (when externally changed)
            self._orig_lst = self._act()
            self._transform()
            assert getattr(self, "_xfm_lst", None) is not None
            self._wdg = SatelliteViewport()
            self._wdg.assign(self._xfm_lst)
        else:
            raise NotImplementedError()

    # VIZ: sort, reverse, filter, groupby, aug/highlight/mark/tag
    def _transform(self) -> None:
        self._xfm_lst = list(self._orig_lst)
        self._apply_default_policy()

    def _apply_default_policy(self) -> None:
        # pylint:disable=protected-access
        # HACK:(this): force cmp with new instance after reload()
        if isinstance(self._ent, this.FSEntry) and fs.isdir(self._ent._x):
            os.chdir(self._ent._x)
            if not fs.islink(self._ent.loci) or self._ent._alt is True:
                self._xfm_lst.sort()


# ENH:ADD: triplet preview (Miller)
class NaviWidget:
    _view: EntityView

    def __init__(self, ent: Representable) -> None:
        # NOTE: we create a separate `SatelliteViewport per each `Entity assigned
        #   NICE: preserve "pos,vh,margin" as-is, and then reinterpret/resize only when going back
        #   ++ NICE: preserve the *generated* items as-is in the "_wdg._lst"
        #   +++ NICE: can use totally different *widgets* based on the type(_ent)
        #     e.g. Dashboard or Editor
        self._view = EntityView(ent)
        # MAYBE:CHG: directly store "ent" in _stack to represent "xpath",
        #   as now we can use "_pool" -- to map it to temporarily cached "_view"
        #   RENAME? _cursor_chain/navi_stack | _pool_cached_view/_view_pool
        self._history_stack = [self._view]
        self._history_idx = len(self._history_stack) - 1
        # NOTE: tba to restore view when opening previously visited nodes
        self._history_pool = {ent: self._view}

    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        # pylint:disable=protected-access
        self._view._wdg.resize(vh, vw, origin=origin)

    def cursor_step_by(self, steps: int) -> None:
        # pylint:disable=protected-access
        self._view._wdg.step_by(steps)

    def view_go_into(self) -> None:
        # pylint:disable=protected-access
        pwdg = self._view._wdg

        # ALT:BET? temporarily insert HaltEntry into all empty lists
        #   BAD: such entry will get superfluous index "01 | 001> "
        #     ~~ it's better to disable indexes for *all* HaltEntry()
        if not pwdg._lst:
            log.trace("EMPTY")
            return

        nent = pwdg.focused_item

        if isinstance(nent, HaltEntry):
            log.trace(nent.name)
            return

        def _histappend() -> None:
            if v := self._history_pool.get(nent):
                self._view = v
            else:
                self._view = EntityView(nent)
                self._history_pool[nent] = self._view
            self._history_stack.append(self._view)
            self._history_idx = len(self._history_stack) - 1

        # NOTE: keep previous navi stack to be able to return to same EntityView
        #   but discard the rest of navi stack if we go into different route
        if (nidx := self._history_idx + 1) < len(self._history_stack):
            if self._history_stack[nidx]._ent == nent:
                self._history_idx = nidx
                self._view = self._history_stack[nidx]
            else:
                del self._history_stack[nidx:]
                _histappend()
        else:
            _histappend()
        # NOTE: resize() newly created wdg to same dimensions
        self._view._wdg.resize(
            pwdg._viewport_height_lines,
            pwdg._viewport_width_columns,
            origin=pwdg._viewport_origin_yx,
        )

    def view_go_back(self) -> None:
        # pylint:disable=protected-access
        pwdg = self._view._wdg
        if self._history_idx > 0:
            self._history_idx -= 1
            self._view = self._history_stack[self._history_idx]
        elif isinstance(self._view._ent, this.FSEntry):
            # RND:(controversial): as we basically navigate to *new* nodes to the left,
            #   so we should keep the history of this navigation, but we discard that
            parent_ent = this.FSEntry(fs.dirname(self._view._ent._x))
            self._view = EntityView(parent_ent)  # , hint_idx=0)
            self._history_stack = [self._view]
            self._history_idx = len(self._history_stack) - 1
        else:
            raise NotImplementedError()
        # NOTE: resize() old/cached wdg, as window may had resized from then.
        self._view._wdg.resize(
            pwdg._viewport_height_lines,
            pwdg._viewport_width_columns,
            origin=pwdg._viewport_origin_yx,
        )

    def redraw(self, stdscr: C.window) -> None:
        # FIXED: prevent crash when window shrinks past the cursor
        # self.cursor_step_by(0)
        # NOTE: actually _lst here stands for a generic _augdbpxy with read.API
        #   i.e. DB augmented by virtual entries, all generated-and-cleared on demand
        self._view._wdg.redraw(stdscr)

    # USE: log.info(str(wdg))
    # def __str__(self) -> str:
    #     s = "  " * 0 + str(0) + ": " + self._ent.name
    #     for i, x in enumerate(self._lstpxy, start=1):
    #         s += "\n  " * 1 + str(i) + ": " + x.name
    #     s += "\r"
    #     s += str(v) if isinstance((v := self._valpxy.get()), int) else repr(v)
    #     return s


class RootWidget:
    _wh: int
    _ww: int

    # USE? {RootWidget(HaltEntry("NOTHING"))} for a "default" ctor
    def __init__(self, ent: Representable) -> None:
        self.set_entity(ent)

    def set_entity(self, ent: Representable) -> None:
        # FUT: may create different widgets based on `Entity and `Policy
        self._navi = NaviWidget(ent)

    ## DISABLED: we need explicit methods for type-checking
    ##   and to appropriately update header/footer on action
    # def action[**P](self, name: str, *args: P.args, **kwargs: P.kwargs) -> None:
    #     getattr(self, name)(*args, **kwargs)

    def cursor_step_by(self, steps: int) -> None:
        # pylint:disable=protected-access
        self._navi.cursor_step_by(steps)

    def view_go_into(self) -> None:
        self._navi.view_go_into()
        # self._invalidate_header_footer()

    def view_go_back(self) -> None:
        self._navi.view_go_back()
        # self._invalidate_header_footer()

    def resize(self, stdscr: C.window) -> None:
        self._wh, self._ww = stdscr.getmaxyx()
        orig_yx = (1, 1)
        size_yx = (self._wh - orig_yx[0] - 1, self._ww - orig_yx[1])
        self._navi.resize(*size_yx, origin=orig_yx)

    def redraw(self, stdscr: C.window) -> None:
        # FUT: dispatch to either curses/cli/Qt
        assert isinstance(stdscr, C.window)
        # FUT: only clear "rest of each line" -- and only if prev line there was longer
        stdscr.clear()
        c_item = C.color_pair(ColorMap.default)
        c_iteminfo = C.color_pair(ColorMap.iteminfo)
        c_auxinfo = C.color_pair(ColorMap.auxinfo)
        c_footer = C.color_pair(ColorMap.footer)
        wdg = self._navi._view._wdg

        # pylint:disable=protected-access
        # ALT:([]): use ⸤⸣ OR ⸢⸥
        header = f"[{self._navi._history_idx+1}⁄{len(self._navi._history_stack)}] "
        stdscr.addstr(0, 0, header, c_auxinfo)
        xpath = wdg.focused_item.loci if wdg._lst else self._navi._view._ent.loci + "/"
        try:
            iname = xpath.rindex("/")
            stdscr.addstr(0, len(header), xpath[:iname], c_footer | C.A_BOLD)
            try:
                ilnum = xpath.index(":", iname)
                stdscr.addstr(0, len(header) + iname, xpath[iname:ilnum], c_item)
                stdscr.addstr(0, len(header) + ilnum, xpath[ilnum:], c_iteminfo)
            except ValueError:
                stdscr.addstr(0, len(header) + iname, xpath[iname:], c_item)
        except ValueError:
            stdscr.addstr(0, len(header), xpath, c_footer | C.A_BOLD)

        self._navi.redraw(stdscr)

        ci = 1 + wdg._cursor_item_lstindex
        sz = len(wdg._lst)
        sortby = "name"
        sortrev = False
        footer = f"--- {ci}/{sz} | by={sortby}{"￪" if sortrev else "￬"}"
        ## DEBUG:NEED:(__main__.py): -X tracemalloc
        # footer += f"  --- {{RAM={__import__("tracemalloc").get_traced_memory()[0]//1024:,}kB}}"
        stdscr.addstr(self._wh - 1, 0, footer, c_footer)

        # NOTE: place real cursor to where list-cursor is, to make tmux overlay selection more intuitive
        cy = wdg._viewport_origin_yx[0]
        if not wdg._lst or isinstance(wdg.focused_item, HaltEntry):
            cx = 0
        else:
            cx = wdg._viewport_origin_yx[1] + 3  # = len(pfx)
        pos = wdg._viewport_followeditem_linesfromtop
        if 0 <= pos < wdg._viewport_height_lines:
            cy += pos
        stdscr.move(cy, cx)


def _live() -> None:
    log.sep()
    from .app import g_app as g
    from .util.exchook import log_exc
    from .widget import RootWidget  # pylint:disable=import-self,redefined-outer-name

    # pylint:disable=protected-access
    # HACK: we should restore all "EntryView" contexts, as we recreate ALL objects
    #   ENH:TODO: serialize/deserialize whole `Views inof only current view pos/idx ?
    hidx = None
    if pnavi := getattr(g.root_wdg, "_navi", None):
        hidx = pnavi._view._wdg._cursor_item_lstindex

    # i: int
    try:
        g.root_wdg = RootWidget(this.FSEntry("/d/airy"))
        # g.root_wdg.set_entity(this.FSEntry("/etc/udev"), hint_idx=hidx)
        g.curses_ui.resize()

        # ALT: fuzzy-test, by random direction of up/down 50 times
        # ndown = 30
        # for i in range(ndown):
        #     wdg.cursor_step_by(1)
        # wdg.redraw(g.stdscr)
        # g.stdscr.refresh()
        # for i in range(ndown):
        #     wdg.cursor_step_by(-1)
        # wdg.redraw(g.stdscr)
        # g.stdscr.refresh()
    except Exception as exc:  # pylint:disable=broad-exception-caught
        # exc.add_note(f"{i=}")
        log_exc(exc)
