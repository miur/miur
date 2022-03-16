from subprocess import PIPE, Popen
from typing import Iterator


# RENAME: -> PacmanItem :: use dif. fragments based on typeof(Item)
class Item:
    def __init__(self, data: dict[str, str]) -> None:
        self._data = data
        self.dldsz: int = None

    @property
    def name(self) -> str:
        return self._data["Name"]

    def __str__(self) -> str:
        rsn = " " if "Explicitly" in self._data["Install Reason"] else "~"
        deps = self._data["Depends On"].split()
        ## FAIL: exceptions in Item are silently ignored
        # opls = self._data["Optional Deps"].splitlines()
        # # FMT: i3lock: for the default screen locker [installed]
        # omap = {p[0]: p[1] for x in opls if (p := x.split(": ", 1))}
        # inst = [v for v in omap.values() if v.endswith(" [installed]")]
        # return f"{len(deps):2d} ({len(inst)}/{len(omap)}) |{rsn}{self.name}"
        sz = self._data["Installed Size"]
        if self.dldsz:
            sz = f"{sz:10,d}"
        return f"{len(deps):2d} {sz:>11s}|{rsn}{self.name}"


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
    from just.iji.shell import runlines

    dldout = runlines(["pacman", "-Su", "--print-format=%s %n"])
    dldszs = {s[1]: int(s[0], 10) for x in dldout if (s := x.split(maxsplit=1))}
    maxsznms = [k for k, v in reversed(sorted(dldszs.items()))]
    # for x in list(parse_pacman("pacman -Qi -".split(), input="\n".join(maxsznms))):
    for x in list(parse_pacman("pacman -Qi".split() + maxsznms)):
        x.dldsz = dldszs[x.name]
        yield x


class DataProvider:
    def __init__(self) -> None:
        # self._items = list(x.rstrip() for x in sys.stdin)
        # USAGE: $ </dev/null M
        # ALT: read_text(/home/amer/aura/items/neo/pacman/odd-pkgs.txt)
        self._items: list[Item] = []
        self.refresh()

    def refresh(self) -> None:
        gen = parse_pacman("pacman -Qti".split())
        # gen = reversed(sorted(parse_pacman_upgrade(), key=lambda x: x.dldsz))
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
