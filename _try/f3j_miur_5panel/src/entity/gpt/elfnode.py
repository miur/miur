from typing import Any, override

from elftools.elf.elffile import ELFFile

from ..base.golden import Entities, Entity, Golden
from ..base.interp import Interp
from ..core.text import TextEntry


class ELFInterp(Interp):
    """Interpretation that parses ELF files."""

    @property
    def name(self) -> str:
        return "elf"

    def can_handle(self, obj: object) -> bool | None:
        if isinstance(obj, str):
            try:
                with open(obj, "rb") as f:
                    return f.read(4) == b"\x7fELF"
            except (OSError, IOError):
                return False
        return False

    def interpret(self, obj: object) -> Entities:
        path = obj if isinstance(obj, str) else None
        if path is None:
            raise ValueError("ELF interpretation requires a file path")
        yield ELFFileEntity(ELFAccessor(path), None)


class ELFAccessor:
    def __init__(self, path: str) -> None:
        self._path = path

    @property
    def handle(self) -> str:
        return self._path

    def __str__(self) -> str:
        import os

        return os.path.basename(self._path)


class ELFFileEntity(Golden[ELFAccessor]):
    """Root entity for an ELF file."""

    __slots__ = ("_elf",)

    def __init__(self, accessor: ELFAccessor, parent: Entity | None) -> None:
        super().__init__(accessor, parent)
        self._elf: ELFFile | None = None

    def _get_elf(self) -> ELFFile:
        if self._elf is None:
            self._elf = ELFFile.load_from_path(self._x.handle)
        return self._elf

    @override
    def explore(self) -> Entities:
        elf = self._get_elf()

        yield ELFHeaderEntity(ELFHeaderAccessor("header", elf.header), self)
        yield ELFSectionsEntity(ELFSectionsAccessor("sections", elf), self)
        yield ELFSegmentsEntity(ELFSegmentsAccessor("segments", elf), self)
        yield ELFDynamicEntity(ELFDynamicAccessor("dynamic", elf), self)
        yield ELFSymbolsEntity(ELFSymbolsAccessor("symbols", elf), self)


class ELFHeaderAccessor:
    def __init__(self, name: str, header: dict) -> None:
        self._name = name
        self._header = header

    def __str__(self) -> str:
        return self._name


class ELFHeaderEntity(Golden[ELFHeaderAccessor]):
    """ELF header as key-value entries."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        header = self._x._header
        for key, value in header.items():
            yield TextEntry(f"{key}: {value}", self)


class ELFSectionsAccessor:
    def __init__(self, name: str, elf: ELFFile) -> None:
        self._name = name
        self._elf = elf

    def __str__(self) -> str:
        return self._name


class ELFSectionsEntity(Golden[ELFSectionsAccessor]):
    """Container for all ELF sections."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        elf = self._x._elf
        for section in elf.iter_sections():
            if section.name:
                yield ELFSectionEntity(ELFSectionAccessor(section.name, section), self)


class ELFSectionAccessor:
    def __init__(self, name: str, section: Any) -> None:
        self._name = name
        self._section = section

    def __str__(self) -> str:
        return self._name


class ELFSectionEntity(Golden[ELFSectionAccessor]):
    """Individual ELF section."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        section = self._x._section

        yield TextEntry(f"sh_type: {section['sh_type']}", self)
        yield TextEntry(f"sh_addr: {hex(section['sh_addr'])}", self)
        yield TextEntry(f"sh_offset: {hex(section['sh_offset'])}", self)
        yield TextEntry(f"sh_size: {section['sh_size']}", self)
        yield TextEntry(f"sh_flags: {hex(section['sh_flags'])}", self)

        if section["sh_size"] > 0 and section["sh_type"] not in (
            "SHT_NOBITS",
            "SHT_NOTE",
        ):
            yield TextEntry("", self)
            yield TextEntry("--- data preview ---", self)
            try:
                data = section.data()
                preview = data[:256] if len(data) > 256 else data
                yield TextEntry(preview.hex(" "), self)
            except Exception as e:
                yield TextEntry(f"(cannot read: {e})", self)


class ELFSegmentsAccessor:
    def __init__(self, name: str, elf: ELFFile) -> None:
        self._name = name
        self._elf = elf

    def __str__(self) -> str:
        return self._name


class ELFSegmentsEntity(Golden[ELFSegmentsAccessor]):
    """Container for all ELF segments."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        elf = self._x._elf
        for segment in elf.iter_segments():
            yield ELFSegmentEntity(
                ELFSegmentAccessor(f"segment_{segment['p_type']}", segment), self
            )


class ELFSegmentAccessor:
    def __init__(self, name: str, segment: Any) -> None:
        self._name = name
        self._segment = segment

    def __str__(self) -> str:
        return self._name


class ELFSegmentEntity(Golden[ELFSegmentAccessor]):
    """Individual ELF segment."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        segment = self._x._segment

        yield TextEntry(f"p_type: {segment['p_type']}", self)
        yield TextEntry(f"p_offset: {hex(segment['p_offset'])}", self)
        yield TextEntry(f"p_vaddr: {hex(segment['p_vaddr'])}", self)
        yield TextEntry(f"p_paddr: {hex(segment['p_paddr'])}", self)
        yield TextEntry(f"p_filesz: {hex(segment['p_filesz'])}", self)
        yield TextEntry(f"p_memsz: {hex(segment['p_memsz'])}", self)
        yield TextEntry(f"p_flags: {hex(segment['p_flags'])}", self)


class ELFDynamicAccessor:
    def __init__(self, name: str, elf: ELFFile) -> None:
        self._name = name
        self._elf = elf

    def __str__(self) -> str:
        return self._name


class ELFDynamicEntity(Golden[ELFDynamicAccessor]):
    """Container for dynamic section."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        elf = self._x._elf
        for segment in elf.iter_segments():
            if segment["p_type"] == "PT_DYNAMIC":
                for tag in segment.iter_tags():
                    yield ELFDynamicTagEntity(
                        ELFDynamicTagAccessor(f"DT_{tag['d_tag']}", tag), self
                    )


class ELFDynamicTagAccessor:
    def __init__(self, name: str, tag: Any) -> None:
        self._name = name
        self._tag = tag

    def __str__(self) -> str:
        return self._name


class ELFDynamicTagEntity(Golden[ELFDynamicTagAccessor]):
    """Individual dynamic tag."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        tag = self._x._tag
        yield TextEntry(f"d_tag: {tag['d_tag']}", self)
        yield TextEntry(f"d_val: {tag['d_val']}", self)
        yield TextEntry(f"d_ptr: {hex(tag['d_ptr'])}", self)


class ELFSymbolsAccessor:
    def __init__(self, name: str, elf: ELFFile) -> None:
        self._name = name
        self._elf = elf

    def __str__(self) -> str:
        return self._name


class ELFSymbolsEntity(Golden[ELFSymbolsAccessor]):
    """Container for all symbol tables."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        elf = self._x._elf
        for section in elf.iter_sections():
            if section["sh_type"] in ("SHT_SYMTAB", "SHT_DYNSYM"):
                if section.name:
                    yield ELFSymbolTableEntity(
                        ELFSymbolTableAccessor(section.name, section, elf), self
                    )


class ELFSymbolTableAccessor:
    def __init__(self, name: str, section: Any, elf: ELFFile) -> None:
        self._name = name
        self._section = section
        self._elf = elf

    def __str__(self) -> str:
        return self._name


class ELFSymbolTableEntity(Golden[ELFSymbolTableAccessor]):
    """Symbol table section."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        section = self._x._section
        for symbol in section.iter_symbols():
            if symbol.name:
                yield ELFSymbolEntity(ELFSymbolAccessor(symbol.name, symbol), self)


class ELFSymbolAccessor:
    def __init__(self, name: str, symbol: Any) -> None:
        self._name = name
        self._symbol = symbol

    def __str__(self) -> str:
        return self._name


class ELFSymbolEntity(Golden[ELFSymbolAccessor]):
    """Individual symbol."""

    __slots__ = ()

    @override
    def explore(self) -> Entities:
        symbol = self._x._symbol

        yield TextEntry(f"st_value: {hex(symbol['st_value'])}", self)
        yield TextEntry(f"st_size: {symbol['st_size']}", self)
        yield TextEntry(f"st_info: {symbol['st_info']}", self)
        yield TextEntry(f"st_shndx: {symbol['st_shndx']}", self)
