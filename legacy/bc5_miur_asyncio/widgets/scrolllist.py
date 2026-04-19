from typing import overload

from ..dom.provider import DataProvider, Item


# TODO: wrap each Item into ScrollItem(Item) to embed position
#   BAD: resort/midinsert => rebuild all ScrollItem objects (slow)
class ScrollListWidget:
    def __init__(self, provider: DataProvider) -> None:
        self._provider = provider
        self._height = 20
        self._width = 80
        # self.offset = 10
        self._offset = 0

    def __len__(self) -> int:
        return len(self._provider)

    @overload
    def __getitem__(self, k: slice) -> list[Item]:
        ...

    @overload
    def __getitem__(self, k: int) -> Item:
        ...

    def __getitem__(self, k: slice | int) -> list[Item] | Item:
        beg, end = self.range(k)
        if isinstance(k, int):
            return self._provider[beg]
        return self._provider[beg:end]

    def range(self, k: slice | int) -> tuple[int, int]:
        if isinstance(k, slice):
            # assert k.stop - k.start <= self.height, k
            beg = k.start + self.offset
            end = min(k.stop + self.offset, beg + self.height)
            return beg, end
        if isinstance(k, int):
            beg = k + self.offset
            return beg, beg + 1
        return NotImplementedError

    @property
    def height(self) -> int:
        return self._height

    @property
    def offset(self) -> int:
        return self._offset

    @offset.setter
    def offset(self, value: int) -> None:
        self._offset = max(0, min(value, len(self._provider) - self.height))


# NOTE: cursor can only be applied to current "window/view" -- whatever is shown on screen
#   BUT: "file selection" is applicable to whole underlying DataProvider
#     WARN: it may even span over multiple folders instead of only current one
#   ALSO: we may pan list to "preview" above/below stationary cursor for e.g. lazy/chat streams
#     ALT: place bookmark for position and then jump to bookmark after cursor scrolling
class CursorViewWidget:
    def __init__(self, widget: ScrollListWidget) -> None:
        self._scroll = widget
        self._curabs = 0
        self._pos = 0
        self._margin = 2

    @overload
    def __getitem__(self, k: slice) -> list[Item]:
        ...

    @overload
    def __getitem__(self, k: int) -> Item:
        ...

    def __getitem__(self, k: slice | int) -> list[Item] | Item:
        return self._scroll[k]

    def __len__(self) -> int:
        return len(self._scroll)

    def resize(self, w: int, h: int) -> None:
        self._scroll._width = w
        self._scroll._height = h

    @property
    def item(self) -> Item:
        return self[self.curabs]

    @property
    def pos(self) -> int:
        return self._pos

    @pos.setter
    def pos(self, y: int) -> None:
        # TODO: relpos, wrappos, abspos -- for <End> = jump_to(-1)
        # TODO: keepcursor i.e. when jumping by "delta y" -- keep onscree cursor at same place
        #   << i.e. scroll list under cursor but keep cursor itself
        # MAYBE: allow "peeking" by scrolling list under cursor, bound to item insted of screen position
        #   << NOTE: cursor may disappear from current screen view and it's ok,
        #     it must simply become "inactive" i.e. disable all destructive funcs
        wg = self._scroll
        vh = wg.height  # = viewport height
        top = 0
        bot = vh - 1
        vm = self._margin
        if y < top:
            delta = y - top
            wg.offset += delta
            y = top
        elif y < vm:
            pass  # y = y
        # TODO: if < self._scroll.offsets -> self._scroll.offset += y - ...
        elif y < vh - vm:
            pass  # y = y
        elif y < vh:
            pass  # y = y
        else:
            # FIXME? only allow y=relative(+N/-N) -- absolute are confusing
            #   << ambiguity: y=40 is absolute position in folder or on screen ?
            delta = y - bot
            wg.offset += delta
            y = bot
        self._pos = y

    @property
    def currel(self) -> int:
        "Cursor position relative to viewport window offset"
        return self.curabs - self.curoff

    # NOTE: ported (sw-offset) and (sw-currel) from /d/miur/lisp/miur.lisp
    #   [_] RENAME! current vars don't have sense
    # ALSO:TODO: sw-curinc -- set pos relative to current cursor position
    # THINK: we don't need currel COS we have "self.pos += 1" BAD not lambda-able
    @currel.setter
    def currel(self, v: int) -> None:
        "Set cursor position relative to viewport"
        self.curabs = self.curoff + v

    @property
    def curabs(self) -> int:
        return self._curabs

    @curabs.setter
    def curabs(self, v: int | None) -> None:
        "Set cursor absolute position relative to snapshot -- and move window"
        keep = self._margin
        beg = 0
        bot = beg + keep
        off = self.curoff
        low = off + keep
        lim = self._scroll.height
        # FIXME: when {lim/wnd < keep*2}
        upp = off + lim - keep - 1
        end = len(self) - 1
        mst = end - lim + 1
        top = end - keep

        if v is None:  # HACK: jump to last element
            wnd, cab = mst, end
        elif v < beg:
            wnd, cab = beg, beg
        elif v < bot:
            wnd, cab = beg, v
        elif v < low:
            wnd, cab = (v - keep), v
        elif v <= upp:
            wnd, cab = off, v
        elif v <= top:
            wnd, cab = (v - lim + keep + 1), v
        elif v <= end:
            wnd, cab = mst, v
        else:
            wnd, cab = mst, end
        if off != wnd:
            self._scroll._offset = wnd
        # FIXME: use dedicate .curabs (like in .lisp) inof relat .pos
        # self._pos = cab - wnd
        self._curabs = cab
        # DEBUG:
        print(' '.join(f"{k}={v}" for k, v in locals().items() if k not in ('self', 'wg')))

    @property
    def curoff(self) -> int:
        return self._scroll.offset

    @curoff.setter
    def curoff(self, v: int | None) -> None:
        "Set window absolute position relative to snapshot -- and move cursor"
        keep = self._margin
        bot = 0
        cur = self.curabs
        off = self.curoff
        low = off + keep
        lim = self._scroll.height
        upp = off + lim - keep - 1
        end = len(self) - 1
        top = end - lim

        if v is None:  # HACK: jump to last element
            wnd, cab = (end - lim), max(low, min(end, cur))
        elif v < bot:
            wnd, cab = bot, max(bot, min(upp, cur))
        elif v <= top:
            wnd, cab = v, max(low, min(upp, cur))
        else:
            wnd, cab = (end - lim), max(low, min(end, cur))

        if off != wnd:
            self._scroll._offset = wnd
        # FIXME: use dedicate .curabs (like in .lisp) inof relat .pos
        self._pos = cab - wnd


# TODO: 3-panel LayoutWidget -- each showing its own ScrollListWidget
