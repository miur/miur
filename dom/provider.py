from subprocess import PIPE, Popen

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
        # self._items = list(map(Item, runlines("pacman -Qtq")))
        self._items = []
        # TODO: lazy parse -- store whole output but parse only during scrolling
        with Popen("pacman -Qti".split(), stdout=PIPE, bufsize=1, text=True) as proc:
            i = 0
            nm = None
            pk = None
            pv = ""
            # for i, l in enumerate(proc.stdout.readlines()):
            assert proc.stdout
            while l := proc.stdout.readline():
                l = l.rstrip()
                if not l and nm:
                    x = Item(nm)
                    nm = None
                    self._items.append(x)
                elif l.startswith(" "):
                    pv += "\n" + l.lstrip()
                else:
                    if pk == "Name":
                        nm = pv
                    try:
                        pk, pv = l.split(" : ", 1)
                    except ValueError as exc:
                        raise ValueError(i, l) from exc
                    else:
                        pk = pk.strip()
                        pv = pv.strip()
                i += 1
            if nm:
                x = Item(nm)
                self._items.append(x)

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
