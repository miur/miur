from subprocess import PIPE, Popen
from typing import Iterator


class Item:
    def __init__(self, data: dict) -> None:
        self._data = data

    @property
    def name(self) -> str:
        return self._data["Name"]

    def __str__(self) -> str:
        rsn = "*" if "Explicitly" in self._data["Install Reason"] else " "
        deps = self._data["Depends On"].split()
        return f"{rsn} {len(deps):2d} | {self.name}"


def parse_pacman() -> Iterator[Item]:
    # from just.iji.shell import runlines
    # self._items = list(map(Item, runlines("pacman -Qtq")))
    # TODO: lazy parse -- store whole output but parse only during scrolling
    with Popen("pacman -Qti".split(), stdout=PIPE, bufsize=1, text=True) as proc:
        i = 0
        dic: dict[str, str] = {}
        pk = ""
        pv = ""
        # for i, l in enumerate(proc.stdout.readlines()):
        assert proc.stdout
        while l := proc.stdout.readline():
            l = l.rstrip()
            if not l and dic:
                dic[pk] = pv
                yield Item(dic)
                dic = {}
            elif l.startswith(" "):
                pv += "\n" + l.lstrip()
            else:
                dic[pk] = pv
                try:
                    pk, pv = l.split(" : ", 1)
                except ValueError as exc:
                    raise ValueError(i, l) from exc
                else:
                    pk = pk.strip()
                    pv = pv.strip()
            i += 1
        if dic:
            yield Item(dic)


class DataProvider:
    def __init__(self) -> None:
        # self._items = list(x.rstrip() for x in sys.stdin)
        # USAGE: $ </dev/null M
        # ALT: read_text(/home/amer/aura/items/neo/pacman/odd-pkgs.txt)
        self._items: list[Item] = []
        self.refresh()

    def refresh(self) -> None:
        self._items = list(parse_pacman())

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
