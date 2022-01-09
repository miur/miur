import sys
from subprocess import run


def runlines(cmd):
    return run(cmd, check=True, capture_output=True, text=True).stdout.splitlines()


class DataProvider:
    def __init__(self) -> None:
        # self._lines = list(x.rstrip() for x in sys.stdin)
        # USAGE: $ </dev/null M
        self._lines = runlines("pacman -Qtq".split())

    def __len__(self) -> int:
        # FUTURE: ret "+Inf" to indicate that it's unbounded stream -- for easier comparisons
        return len(self._lines)

    def __getitem__(self, k: slice) -> list[str]:
        if isinstance(k, slice):
            # assert k.stop < len(self._lines)
            beg = k.start
            end = k.stop
            return self._lines[beg:end]
        if isinstance(k, int):
            return self._lines[k]
        return NotImplementedError


class ScrollListWidget:
    def __init__(self, provider: DataProvider = None) -> None:
        self._provider = provider or DataProvider()
        self._height = 20
        self._offset = 10

    def __getitem__(self, k: slice) -> list[str]:
        if isinstance(k, slice):
            # assert k.stop - k.start <= self.height, k
            beg = k.start + self.offset
            end = min(k.stop + self.offset, beg + self.height)
            return self._provider[beg:end]
        if isinstance(k, int):
            return self._provider[k + self.offset]
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
class CursorViewWidget:
    def __init__(self, widget: ScrollListWidget = None) -> None:
        self._scroll = widget or ScrollListWidget()
        self._pos = 0
        self._margin = 3

    def __getitem__(self, k: slice) -> list[str]:
        return self._scroll[k]

    @property
    def pos(self) -> int:
        return self._pos

    @pos.setter
    def pos(self, y: int) -> None:
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
