from abc import ABCMeta, abstractmethod
from typing import (
    Annotated,
    Any,
    Callable,
    Iterable,
    Iterator,
    Protocol,
    Sequence,
    TypeVar,
)

# pylint:disable=too-few-public-methods

T = TypeVar("T")
_Tx_co = TypeVar("_Tx_co", covariant=True)
_Tx_contra = TypeVar("_Tx_contra", contravariant=True)
_REntity_co = TypeVar("_REntity_co", bound="Entity[Any]", covariant=True)


########################################
class Composable(Protocol[_Tx_co]):
    def __mul__(self, sfn: Callable[[_Tx_co], _REntity_co], /) -> _REntity_co: ...


# Entity[Inner] = Pointed + Composable + Named
class Entity(Composable[_Tx_co], metaclass=ABCMeta):
    def __init__(self, x: _Tx_co, /) -> None:
        self._x = x

    # @abstractmethod
    def __mul__(self, sfn: Callable[[_Tx_co], _REntity_co], /) -> _REntity_co:
        return sfn(self._x)

    @property
    def name(self) -> str:
        return repr(self._x)


########################################
class Applicable(Protocol[_Tx_contra]):
    def __call__(self, x: _Tx_contra, /) -> Composable[Any]: ...


# Action = Entity[Context] + SpecialFunction
class Action(Entity[_Tx_co], Applicable[_Tx_contra]):
    @abstractmethod
    def __call__(self, x: _Tx_contra, /) -> Entity[Any]: ...


########################################
# EVO: CachedAsyncListingProxy with non-"list" IMPL
class CachedListing(Entity[Sequence[T]]):
    def __init__(self, xs: Iterable[T]) -> None:
        super().__init__(list(xs))

    def __iter__(self) -> Iterator[T]:
        return iter(self._x)


class GetByIndex(Action[int, Sequence[Entity[Any]]]):
    def __call__(self, xs: Sequence[_REntity_co]) -> _REntity_co:
        return xs[self._x]


########################################
class FSEntry(Entity[str]):
    _x: Annotated[str, "Path"]
    # def __init__(self, path: str) -> None:
    #     super().__init__(path)


class ListFSEntries(Action[None, str]):
    def __call__(self, path: str) -> CachedListing[FSEntry]:
        with __import__("os").scandir(path) as it:
            return CachedListing(FSEntry(e.path) for e in it)


########################################
class Keyval(Entity[tuple[str, _Tx_co]]):
    def __init__(self, k: str, v: _Tx_co) -> None:
        super().__init__((k, v))

    @property
    def name(self) -> str:
        return self._x[0] + ": " + repr(self._x[1])


class ListFSStats(Action[None, str]):
    def __call__(self, path: str) -> CachedListing[Keyval[int]]:
        st = __import__("os").lstat(path)
        return CachedListing(
            Keyval(k, getattr(st, k)) for k in dir(st) if k.startswith("st_")
        )


########################################
class PrinterDevice:
    def add_line(self, x: int, y: int, s: str) -> None:
        print("  " * x + str(y) + ": " + s)


# NOTE: generalized viewer COS it may contain mix of different `*Entities
class ListWidget:
    _ent: FSEntry
    _lst: CachedListing[FSEntry]

    def __init__(self, ent: FSEntry) -> None:
        self.set_entity(ent)

    def set_entity(self, ent: FSEntry) -> None:
        self._ent = ent
        self._lst = ent * ListFSEntries(None)

    def __getitem__(self, idx: int) -> FSEntry:  # TEMP:API
        return self._lst * GetByIndex(idx)

    def render_to(self, odev: PrinterDevice) -> None:
        odev.add_line(0, 0, self._ent.name)
        for i, ent in enumerate(self._lst, start=1):
            odev.add_line(1, i, ent.name)


########################################
def _live() -> None:
    wdg = ListWidget(FSEntry("/etc/udev"))
    odev = PrinterDevice()
    wdg.render_to(odev)
    wdg.set_entity(wdg[1])
    wdg.render_to(odev)
