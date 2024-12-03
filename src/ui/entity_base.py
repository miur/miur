from abc import ABCMeta
from typing import Any, Iterable, Protocol, Self


class Representable(Protocol):
    @property
    def name(self) -> str: ...

    # MOVE? "Sortable" ?
    def __lt__(self, other: Self) -> bool:
        return self.name < other.name


class Addressable(Protocol):
    @property
    def loci(self) -> str: ...

    # BAD: we can either sort by loci or by name
    # def __lt__(self, other: Self) -> bool:
    #     return self.loci < other.loci


## ARCH:
#  * on ERROR -> return [`ErrorEntry], mixed with regular entries
#    >> if whole list ~can't be read~ -- it will result in empty list with error
#    COS: we may get multiple errors, for e.g. unreadable elements in the list
#  * on empty list -> return [], and interpret it based on `*Entry itself
#    e.g. to make different messages for empty folder and empty file
#  * if entry is atomic -> return "None", and again interpret it based on `*Entry
#    COS: behavior of HaltEntry is NOT inherent and depends on how we decide to interpret it
#    ALT:BET? remove the method itself and use getattr() to verify its presence
# RENAME? .browse()
class Explorable(Protocol):
    def explore(self) -> "Iterable[Golden]": ...


class Atomic(Addressable, Representable, Protocol):
    __slots__ = ()

    # RENAME? "ATOMIC" | "INTERPRETATION NOT ASSIGNED" | "NO INTERPRETATION" (for blob)
    explore: str = "NOT EXPLORABLE (YET)"


g_entries_cls: "list[Golden]" = []


class Golden(Explorable, Addressable, Representable, Protocol, metaclass=ABCMeta):
    __slots__ = ()

    def __new__(cls, *_args: Any, **_kwds: Any) -> Self:
        # OFF:REF:(no args/kwds): https://mail.python.org/pipermail/python-dev/2008-February/076854.html
        # def __new__[**P](cls, *_args: P.args, **_kwds: P.kwargs) -> Self:
        # BAD:(false-positive): https://github.com/pylint-dev/pylint/issues/8325
        obj = super().__new__(cls)
        g_entries_cls.append(obj)
        return obj
