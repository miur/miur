from subprocess import PIPE, Popen
from typing import Any, Callable, Iterator, Literal


# RENAME: -> PacmanItem :: use dif. fragments based on typeof(Item)
class Item:
    BYTESFX = {"B": 1 << 0, "KiB": 1 << 10, "MiB": 1 << 20, "GiB": 1 << 30}

    def __init__(self, data: dict[str, str]) -> None:
        self._data = data
        self.dldsz: int | None = None  # ~Upgrade Download Size~

    def __getitem__(self, k: str) -> str:
        if k == "nm":
            return self.name
        if k == "sz":
            return self.size
        if k == "bday":
            return self["Build Date"]
        if k == "iday":
            return self["Install Date"]
        return self._data[k]

    @property
    def name(self) -> str:
        return self["Name"]

    @property
    def size(self) -> int:
        if self.dldsz:
            return self.dldsz
        snum, _, sfx = self["Installed Size"].partition(" ")
        return int(float(snum) * self.BYTESFX[sfx])

    @property
    def reason(self) -> str:
        return " " if "Explicitly" in self._data["Install Reason"] else "~"

    @property
    def deps(self) -> list[str]:
        sdep = self._data["Depends On"]
        return sdep.split() if sdep != "None" else []

    @property
    def opls(self) -> tuple[list[str], list[str]]:
        sopl = self._data["Optional Deps"]
        opls = sopl.splitlines() if sopl != "None" else []
        ## FMT: i3lock: for the default screen locker [installed]
        omap = {
            p[0]: p[2]
            for x in opls
            if (p := x.partition(": "))[2] or (p := x.partition(" "))
        }
        ## DEBUG
        # if any(x == "" for x in omap.values()):
        #     print(self.name, omap)
        inst = [v for v in omap.values() if v.endswith("[installed]")]
        return list(omap), inst

    def strdepsnum(self) -> str:
        opls, inst = self.opls
        return f"{len(self.deps):2d} ({len(inst)}/{len(opls)})"

    def strsize(self) -> str:
        return f"{self.size:>11,d}"

    def strnamedecor(self) -> str:
        return f"{self.reason}{self.name}"

    def __str__(self) -> str:
        return self.strnamedecor()


def parse_pacman(cmdargs: list[str]) -> Iterator[Item]:
    # from just.iji.shell import runlines
    # self._items = list(map(Item, runlines("pacman -Qtq")))
    # TODO: lazy parse -- store whole output but parse only during scrolling
    with Popen(cmdargs, stdout=PIPE, bufsize=1, text=True) as proc:
        i = 0
        dic: dict[str, str] = {}
        pk = ""
        pv = ""
        # for i, l in enumerate(proc.stdout.readlines()):
        assert proc.stdout
        while (l := proc.stdout.readline()) or proc.poll() is None:
            l = l.rstrip()
            if not l and not dic:
                continue
            if not l:
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


def parse_pacman_upgrade() -> Iterator[Item]:
    from just.use.iji.shell import runlines

    dldout = runlines(["pacman", "-Su", "--print-format=%s %n"])
    dldszs = {s[1]: int(s[0], 10) for x in dldout if (s := x.split(maxsplit=1))}
    maxsznms = [k for k, v in reversed(sorted(dldszs.items()))]
    # for x in list(parse_pacman("pacman -Qi -".split(), input="\n".join(maxsznms))):
    for x in list(parse_pacman("pacman -Qi".split() + maxsznms)):
        x.dldsz = dldszs[x.name]
        yield x


def parse_pacman_stdin() -> Iterator[Item]:
    names = [x.rstrip() for x in __import__("sys").stdin]
    # for x in list(parse_pacman("pacman -Qi -".split(), input="\n".join(maxsznms))):
    return parse_pacman("pacman -Qi".split() + names)


class DataProvider:
    sortfields = ["nm", "sz", "bday", "iday"]

    def __init__(self) -> None:
        # self._items = list(x.rstrip() for x in sys.stdin)
        # USAGE: $ </dev/null M
        # ALT: read_text(/home/amer/aura/items/neo/pacman/odd-pkgs.txt)
        self._items: list[Item] = []
        self._sortby = self.sortfields[0]
        self._sortrev = False
        self.refresh()

    def refresh(self) -> None:
        gen = parse_pacman("pacman -Qti".split())
        # gen = reversed(sorted(parse_pacman_upgrade(), key=lambda x: x.dldsz))
        # gen = parse_pacman_stdin()
        self._items = list(gen)

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

    def sortby(
        self,
        arg: str | int | Literal["+", "-"] | Callable[[Item], Any] | None = None,
        rev: bool | int | Literal["!"] | None = None,
    ) -> None:
        if arg is None:
            field = self._sortby
        elif isinstance(arg, int):
            field = self.sortfields[arg]
        elif isinstance(arg, str):
            if arg in ("+", "-"):
                num = len(self.sortfields)
                idx = self.sortfields.index(self._sortby)
                idx += 1 if arg == "+" else -1
                if idx >= num:
                    idx -= num
                elif idx < 0:
                    idx += num
                field = self.sortfields[idx]
            else:
                field = arg
        ## BAD: breaks next/prev ops for stored args
        # elif callable(arg):
        #     field = arg
        else:
            raise NotImplementedError

        if rev is None or rev == 0:
            reverse = self._sortrev
        elif isinstance(rev, int):
            reverse = rev > 0
        elif isinstance(rev, bool):
            reverse = rev
        elif isinstance(rev, str) and rev == "!":
            reverse = not self._sortrev
        else:
            raise NotImplementedError

        key = field if callable(field) else lambda x, k=field: x[k]
        self._items.sort(key=key, reverse=reverse)  # type:ignore

        if field != self._sortby:
            self._sortby = field
        if reverse != self._sortrev:
            self._sortrev = reverse
