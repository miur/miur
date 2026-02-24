import os
import os.path as fs
from functools import lru_cache
from typing import Iterable, override

# from ..util.logger import log
from ..base.golden import Accessor, Entities, Entity, Golden
from .text import TextEntry


class FSAccessor(Accessor):
    def __init__(self, path: str) -> None:
        self._path = path  # OR: .fd

    @property
    def handle(self) -> str:
        return self._path

    @override
    @lru_cache(1)
    def __str__(self) -> str:
        # BET? make FSRoot node with fixed .name=file:///
        nm = "/" if self._path == "/" else fs.basename(self._path)
        assert nm
        return nm


# OLD:ALT: FSEntity, FSEntityAdapter, Path2Entity, InterpretAsFSEntity
class FSEntry(Golden[FSAccessor]):
    __slots__ = ()

    # def __init__(self, path: str, nm: bool | str = True, alt: bool = False) -> None:
    def __init__(self, path: str, parent: Entity) -> None:
        # self._alt = alt
        # _nm = fs.basename(path) if nm is True else path if nm is False else nm
        super().__init__(FSAccessor(path), parent)

    # @override
    # @property
    # def loci(self) -> str:
    #     return self._x.path

    @override
    def explore(self) -> Entities:
        raise NotImplementedError(f"TBD: `{self.__class__.__name__}")

    def open_by(self) -> Entities:
        from ..integ.any_exe import run_bg_wait
        from .objaction import ObjAction

        return [
            ObjAction(
                name="xdg",
                parent=self,
                fn=lambda: run_bg_wait(["xdg-open", self._x.handle]),
                allowpreview=False,
            )
        ]

    ## FAIL: `Entry.parent() is not generalizable ※⡧⢃⠬⢖
    def parents_loci(self) -> Iterable[str]:
        assert fs.isabs(self._x.handle)
        path = fs.normpath(self._x.handle)
        if path != "/":
            yield "/"
        k = 0
        while (k := path.find(fs.sep, k + 1)) > 0:
            yield path[:k]

    def stat(self) -> Iterable[str]:
        h = self._x.handle
        st = os.lstat(h)
        # TODO: time.strftime(format, unixts) if nm.endswith("time")
        return [f"{nm}: {getattr(st, nm)}" for nm in dir(st) if nm.startswith("st_")]


def FSAuto(x: os.DirEntry[str] | str, parent: Entity) -> FSEntry:
    cls: type[FSEntry]
    if isinstance(x, os.DirEntry):
        if x.is_symlink():
            cls = FSLink
        elif x.is_dir(follow_symlinks=False):
            cls = FSDir
        elif x.is_file(follow_symlinks=False):
            cls = FSFile
        else:
            cls = FSEntry
        return cls(x.path, parent)

    if isinstance(x, str):
        if fs.islink(x):
            cls = FSLink
        elif fs.isdir(x):
            cls = FSDir
        elif fs.isfile(x):
            cls = FSFile
        else:
            cls = FSEntry
        return cls(x, parent)

    return NotImplementedError(x)


class FSDir(FSEntry):
    @override
    def explore(self) -> Entities:
        h = self._x.handle
        assert fs.isdir(h) and not fs.islink(h)  # MOVE: to ctor()
        with os.scandir(h) as it:
            for e in it:
                yield FSAuto(e, self)


# ALT? replace by [FSDir|FSFile](..., link=True)
#   &why we too often need to distinguish .isdir from .isfile for each FSLink
class FSLink(FSEntry):
    # i.e. =InterpretUnchangedDirListingPropertyAsFSEntriesInUsualWay
    @override
    def explore(self) -> Entities:
        h = self._x.handle
        # if not fs.lexists(p):
        #     return [ErrorEntry("FILE NOT FOUND", loci=(p,))]
        # if not os.access(p, os.R_OK):
        #     return [ErrorEntry("PERMISSION DENIED", loci=(p,))]
        # if not fs.exists(p):
        #     return [ErrorEntry("DANGLING SYMLINK", loci=(fs.realpath(p),))]
        # cls = type(self)
        # [_] TRY: print this info on 2nd/3rd line below the link, as auxinfo
        #   BAD: we are losing unified access to copy/edit/navi the interpreted symlink values as regular items
        #     ALT:NICE: with subcursor we could navi/copy even those 2nd/3rd lines of info
        # [?] ALT:FIXME: skip .islink if .originator==self
        #   NICE: we can walk up through .originator chain and construct full "loci" for pieces
        #   ALT: produce virtual FSEntryLike entry, as it's a collection of paths inof real folder
        #     NICE: preserves original order inof being sorted by default as all other FSEntry
        assert fs.islink(h)
        cls = FSDir if fs.isdir(h) else FSFile if fs.isfile(h) else FSEntry
        return [
            cls(fs.realpath(h), self),
            TextEntry(self.readlink(), self),
            # cls(p, nm=False, alt=True),
            # cls(os.readlink(p), nm=False),
            # cls(fs.realpath(p), nm=False),
            # cls(fs.relpath(fs.realpath(p), p), nm=False),
        ]

    def readlink(self) -> str:
        return os.readlink(self._x.handle)

    def realpath(self) -> str:
        return fs.realpath(self._x.handle)

    def relpath(self) -> str:
        return fs.relpath(self._x.handle)


class FSFile(FSEntry):
    # MOVE:(body): to `Interpret[File|Buffer]AsCodeSyntax/Lines
    def codesyntax_lines(self) -> Entities:
        """TEMP:XLR: syntax-hi for files"""
        p = self._x.handle

        with open(p, "r", encoding="utf-8") as f:
            code = f.read(4096)

        if p.endswith(".py"):
            if __import__("sys").flags.isolated:
                # lazy init for "site" in isolated mode
                __import__("site").main()
            from pygments import highlight
            from pygments.formatters.terminal256 import Terminal256Formatter
            from pygments.lexers.python import PythonLexer

            lexer = PythonLexer()
        else:
            raise NotImplementedError(p)

        # from pygments.style import Style
        # from pygments.token import Token
        # class MyStyle(Style):
        #     styles = {
        #         Token.String: "ansibrightblue bg:ansibrightred",
        #     }
        # fmtr = Terminal256Formatter(style=MyStyle)
        fmtr = Terminal256Formatter()

        # ALT? render into HTML -> load as tree -> walk it and translate to curses
        result = highlight(code, lexer, fmtr)
        # DEBUG: log.trace(result)
        # os.environ["CODE"] = result
        return [TextEntry(x, self) for x in result.split(os.linesep)]

    # MOVE:(body): to `Interpret[File|Buffer]AsPlainText/Lines
    def plaintext_lines(self) -> Entities:
        with open(self._x.handle, "r", encoding="utf-8") as f:
            i = 1
            # ALT:(python>=3.13): lines = f.readlines(sizehint=4096, keepends=False)
            while (boff := f.tell()) < 4096 and (line := f.readline(4096)):
                assert line.endswith(os.linesep), "DECI: line is longer than 4096"
                # DISABLED(, f"  `{boff}"): interferes with !nvim jumping to line under cursor
                yield TextEntry(
                    line.removesuffix(os.linesep), self
                )  # , loci=(h, f":{i}"))
                i += 1

    # MOVE:(body): to `Interpret[File|Buffer]AsHexDump/Lines
    # TODO: on redraw() show "file offset in hex" inof "item idx in _xfm_list"
    def hexdump_lines(self) -> Entities:
        with open(self._x.handle, "rb") as blob:
            i = 1
            while (boff := blob.tell()) < 1024 and (data := blob.read(16)):
                yield TextEntry(
                    data.hex(" "), self
                )  # , loci=(h, f" `0x{boff:x}  #{i}"))
                i += 1

    @override
    def explore(self) -> Entities:
        h = self._x.handle
        assert fs.isfile(h) and not fs.islink(h)  # MOVE:(check once): to ctor()
        # BAD?=ARCH;PERF? we use exceptions for regular control flow with fallbacks here...
        try:
            yield from self.codesyntax_lines()
        except NotImplementedError:
            try:
                yield from self.plaintext_lines()
            except UnicodeDecodeError:
                yield from self.hexdump_lines()
                # else [ErrorEntry("UnicodeDecodeError / NoAccess")]
