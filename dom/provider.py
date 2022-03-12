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
        self._items: list[Item] = []
        self.refresh()

    def refresh(self) -> None:
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
