from typing import Iterable, Protocol, Self


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
    def explore(self) -> Iterable[Representable]: ...


class Atomic(Addressable, Representable, Protocol):
    __slots__ = ()

    # RENAME? "ATOMIC" | "INTERPRETATION NOT ASSIGNED" | "NO INTERPRETATION" (for blob)
    explore: str = "NOT EXPLORABLE (YET)"


class Golden(Explorable, Addressable, Representable, Protocol):
    __slots__ = ()
