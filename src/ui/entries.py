import os
import os.path as fs
from typing import Iterable, override

# from ..util.logger import log
from .entity_base import Golden


# class ErrorEntry(HaltEntry(Atomic))
class ErrorEntry(Golden):
    def __init__(self, msg: str, loci: tuple[str, ...] | None = None) -> None:
        self._msg = msg
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
    def explore(self) -> Iterable[Golden]:
        raise NotImplementedError(self._msg)


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
    def explore(self) -> Iterable[Golden]:
        from re import finditer

        cls = type(self)
        words = [
            cls(
                m.group(0),
                loci=(
                    (
                        ## DISABLED: interferes with !nvim jumping to line under cursor
                        # self._at[0],
                        # f":{m.start()}+{m.end() - m.start()}",
                        # self._at[-1],
                        *self._at,
                        f":{m.start()+1}",
                    )
                    if self._at
                    else ("∅", f":{m.start()}+{m.end() - m.start()}")
                ),
            )
            for m in finditer(r"\S+", self._x)
        ]
        if len(words) == 1:
            return [ErrorEntry("INDIVISIBLE WORD", loci=self._at)]
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
    def explore(self) -> Iterable[Golden]:
        p = self._x
        if not fs.lexists(p):
            return [ErrorEntry("FILE NOT FOUND", loci=(p,))]
        if not os.access(p, os.R_OK):
            return [ErrorEntry("PERMISSION DENIED", loci=(p,))]
        if not fs.exists(p):
            return [ErrorEntry("DANGLING SYMLINK", loci=(fs.realpath(p),))]
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
                return [cls(e.path) for e in it]

        if fs.isfile(p):
            try:
                linelst: list[TextEntry] = []
                with open(p, "r", encoding="utf-8") as f:
                    # TEMP:XLR: syntax-hi for files
                    if p.endswith(".py"):
                        if __import__("sys").flags.isolated:
                            # lazy init for "site" in isolated mode
                            __import__("site").main()
                        from pygments import highlight
                        from pygments.formatters.terminal256 import Terminal256Formatter
                        from pygments.lexers.python import PythonLexer

                        # from pygments.style import Style
                        # from pygments.token import Token
                        # class MyStyle(Style):
                        #     styles = {
                        #         Token.String: "ansibrightblue bg:ansibrightred",
                        #     }
                        # fmtr = Terminal256Formatter(style=MyStyle)

                        code = f.read(4096)
                        # ALT? render into HTML -> load as tree -> walk it and translate to curses
                        result = highlight(code, PythonLexer(), Terminal256Formatter())
                        # DEBUG: log.trace(result)
                        # os.environ["CODE"] = result
                        linelst = [TextEntry(x) for x in result.split("\n")]
                    else:
                        i = 1
                        # ALT:(python>=3.13): lines = f.readlines(sizehint=4096, keepends=False)
                        while (boff := f.tell()) < 4096 and (line := f.readline(4096)):
                            # DISABLED(, f"  `{boff}"): interferes with !nvim jumping to line under cursor
                            ent = TextEntry(line.removesuffix("\n"), loci=(p, f":{i}"))
                            linelst.append(ent)
                            i += 1
                return linelst
            except UnicodeDecodeError:
                # TODO: on redraw() show "file offset in hex" inof "item idx in _xfm_list"
                hexlst: list[TextEntry | ErrorEntry] = []
                with open(p, "rb") as blob:
                    i = 1
                    while (boff := blob.tell()) < 1024 and (data := blob.read(16)):
                        ent = TextEntry(data.hex(" "), loci=(p, f" `0x{boff:x}  #{i}"))
                        hexlst.append(ent)
                        i += 1
                return (
                    hexlst if hexlst else [ErrorEntry("UnicodeDecodeError / NoAccess")]
                )
        raise NotImplementedError(p)
