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


class Explorable(Protocol):
    def explore(self) -> Iterable[Representable]: ...


class Golden(Representable, Addressable, Explorable, Protocol):
    __slots__ = ()
