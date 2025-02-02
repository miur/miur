from functools import cached_property
from typing import TYPE_CHECKING, Iterable, Protocol, override

from .autodiscover import AutoRegistered

if TYPE_CHECKING:
    from ...ui.view import EntityView


# [SystemObject]Accessor/Proxy = .handle(AddressOfStorage) + BackendDataProviderSystem
#   ++ (LineBasedInterpreter+Selector) + StrTypeConverter
#   * UpdateProtocol | .update/invalidate function | event stream
#   * allows e.g. all FS ops
class Accessor(Protocol):
    # RENAME? .get() or .read() | .getdata .readstr .get(type(str))
    @override
    def __str__(self) -> str: ...


# RENAME? `Entity `Node `Discoverable
# ALT:(Protocol):NEED:BAD:PERF:(@runtime_checkable):COS:TEMP: to focus_on(match-case Golden())
class Golden(AutoRegistered):
    __slots__ = ()

    def __init__(self, x: Accessor, pview: "EntityView") -> None:
        self._x = x
        self._pv = pview

    @property
    def name(self) -> str:
        # FUT:PERF:CMP: use @cached_property vs @property+@lru_cache(1) vs .update(self._name) method
        return str(self._x)

    def explore(self) -> "Iterable[Golden]":
        # NOTE: it's reasonable to raise inof using @abc.abstractmethod
        #   * rarely we may have atomic leafs, but for most it's "NOT YET IMPLEMENTED"
        #   * we will use try-catch around .explore() anyway -- to catch errors
        raise NotImplementedError("Atomic?")

    @property
    def pv(self) -> "EntityView":
        return self._pv

    # REMOVE? -> construct `Loci on demand through external means
    @cached_property
    def loci(self) -> str:
        # BAD:PERF: recursive
        parent = "" if self._pv is None else self._pv._ent.loci
        # FAIL:TEMP: "self.name" -> "self._x.selector_for_interpreter"
        return parent + "/" + self.name

    def __lt__(self, other: "Golden") -> bool:
        return self.name < other.name

    @override
    def __repr__(self) -> str:
        loci = self.loci
        if not loci.endswith(nm := self.name):
            loci = nm + loci
        if loci.startswith("/"):
            return f"`{loci}"
        return f"{type(self).__name__}({loci})"


## FIXED:ERR: `FSEntry [too-many-ancestors] Too many ancestors (8/7)
# REF: Is there a way to tell mypy to check a class implements a Protocol without inheriting from it? ⌇⡧⢟⠦⡴
#   https://github.com/python/mypy/issues/8235
if TYPE_CHECKING:  # WARN: we rely on !mypy to verify this (inof runtime checks)
    from . import traits as TR

    # OR:(check for instantiation): _: Standart = Golden()
    _: type[TR.Standart] = Golden
