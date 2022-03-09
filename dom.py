from just.iji.shell import runlines


class Item:
    def __init__(self, data: str) -> None:
        self._data = data

    def __str__(self) -> str:
        return str(self._data)


class DataProvider:
    def __init__(self) -> None:
        # self._items = list(x.rstrip() for x in sys.stdin)
        # USAGE: $ </dev/null M
        # ALT: read_text(/home/amer/aura/items/neo/pacman/odd-pkgs.txt)
        self._items = list(map(Item, runlines("pacman -Qtq")))

    def __len__(self) -> int:
        # FUTURE: ret "+Inf" to indicate that it's unbounded stream -- for easier comparisons
        return len(self._items)

    def __getitem__(self, k: slice) -> list[Item]:
        if isinstance(k, slice):
            # assert k.stop < len(self._items)
            beg = k.start
            end = k.stop
            return self._items[beg:end]
        if isinstance(k, int):
            return self._items[k]
        return NotImplementedError


# TODO: wrap each Item into ScrollItem(Item) to embed position
#   BAD: resort/midinsert => rebuild all ScrollItem objects (slow)
class ScrollListWidget:
    def __init__(self, provider: DataProvider = None) -> None:
        self._provider = provider or DataProvider()
        self._height = 20
        self._offset = 10

    def __len__(self) -> int:
        return len(self._provider)

    def __getitem__(self, k: slice) -> list[Item]:
        beg, end = self.range(k)
        if isinstance(k, int):
            return self._provider[beg]
        return self._provider[beg:end]

    def range(self, k: slice) -> tuple[int, int]:
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
    def __init__(self, widget: ScrollListWidget = None) -> None:
        self._scroll = widget or ScrollListWidget()
        self._pos = 0
        self._margin = 3

    def __getitem__(self, k: slice) -> list[Item]:
        return self._scroll[k]

    def __len__(self) -> int:
        return len(self._scroll)

    def resize(self, w: int, h: int) -> None:
        self._scroll._width = w
        self._scroll._height = h

    @property
    def item(self) -> Item:
        return self[self.pos]

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
            y = y
        # TODO: if < self._scroll.offsets -> self._scroll.offset += y - ...
        elif y < vh - vm:
            y = y
        elif y < vh:
            y = y
        else:
            # FIXME? only allow y=relative(+N/-N) -- absolute are confusing
            #   << ambiguity: y=40 is absolute position in folder or on screen ?
            delta = y - bot
            wg.offset += delta
            y = bot
        self._pos = y


# TODO: 3-panel LayoutWidget -- each showing its own ScrollListWidget
