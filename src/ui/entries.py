import os
import os.path as fs
from typing import Iterable, override

from .entity_base import Golden, Representable


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
        if not words:
            return [HaltEntry("EMPTY TEXT", loci=self._at)]
        if len(words) <= 1:
            # ALT: "NOT EXPLORABLE (YET)" | "INTERPRETATION NOT ASSIGNED"
            return [HaltEntry("ATOMIC", loci=self._at)]
        return words


class FSEntry(Golden):
    __slots__ = ()  # "name", "loci", "explore")

    def __init__(self, path: str, nm: bool | str = True, alt: bool = False) -> None:
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
            return [
                cls(p, nm=False, alt=True),
                cls(os.readlink(p), nm=False),
                cls(fs.realpath(p), nm=False),
                cls(fs.relpath(fs.realpath(p), p), nm=False),
            ]
        if fs.isdir(p):
            with os.scandir(p) as it:
                fslst: list[FSEntry | HaltEntry] = []
                fslst = [cls(e.path) for e in it]
                return fslst if fslst else [HaltEntry("EMPTY DIR")]

        if fs.isfile(p):
            try:
                linelst: list[TextEntry | HaltEntry] = []
                with open(p, "r", encoding="utf-8") as f:
                    # ALT:(python>=3.13): lines = f.readlines(sizehint=1024, keepends=False)
                    i = 1
                    while (boff := f.tell()) < 1024 and (line := f.readline(1024)):
                        ent = TextEntry(
                            line.removesuffix("\n"), loci=(p, f":{i}", f"  `{boff}")
                        )
                        linelst.append(ent)
                        i += 1
                return linelst if linelst else [HaltEntry("EMPTY FILE")]
            except UnicodeDecodeError:
                # TODO: on redraw() show "file offset in hex" inof "item idx in _xfm_list"
                hexlst: list[TextEntry | HaltEntry] = []
                with open(p, "rb") as f:
                    i = 1
                    while (boff := f.tell()) < 1024 and (data := f.read(16)):
                        ent = TextEntry(data.hex(" "), loci=(p, f" `0x{boff:x}  #{i}"))
                        hexlst.append(ent)
                        i += 1
                return (
                    hexlst if hexlst else [HaltEntry("UnicodeDecodeError / NoAccess")]
                )
        raise NotImplementedError(p)
