from typing import (
    TYPE_CHECKING,
    Callable,
    Generic,
    Iterable,
    Iterator,
    Sequence,
    TypeVar,
)

if TYPE_CHECKING:
    from typing import Any
else:
    reveal_type = print

# pylint:disable=too-few-public-methods

T = TypeVar("T")
_Tx_co = TypeVar("_Tx_co", covariant=True)
_REntity_co = TypeVar("_REntity_co", bound="Entity[Any]", covariant=True)


# Entity = Pointed + Named + Composable
class Entity(Generic[_Tx_co]):

    def __init__(self, x: _Tx_co) -> None:
        self._x = x

    @property
    def name(self) -> str:
        return repr(self._x)

    def __mul__(self, fsp: Callable[[_Tx_co], _REntity_co], /) -> _REntity_co:
        return fsp(self._x)


# EVO: CachedAsyncListingProxy with non-"list" IMPL
class CachedListing(Entity[Sequence[T]]):
    def __init__(self, xs: Iterable[T]) -> None:
        super().__init__(list(xs))

    def __iter__(self) -> Iterator[T]:
        return iter(self._x)


class GetByIndexAction(Entity[int]):
    def __call__(self, xs: Sequence[_REntity_co]) -> _REntity_co:
        return xs[self._x]


class FSEntry(Entity[str]):
    # _x: Annotated[str, "Path"]
    def __init__(self, path: str) -> None:
        super().__init__(path)


class ListFSEntriesAction(Entity[None]):
    def __call__(self, path: str) -> CachedListing[FSEntry]:
        with __import__("os").scandir(path) as it:
            return CachedListing(FSEntry(e.path) for e in it)


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
        self._lst = ent * ListFSEntriesAction(None)

    def __getitem__(self, idx: int) -> FSEntry:  # TEMP:API
        return self._lst * GetByIndexAction(idx)

    def render_to(self, odev: PrinterDevice) -> None:
        odev.add_line(0, 0, self._ent.name)
        for i, ent in enumerate(self._lst, start=1):
            odev.add_line(1, i, ent.name)


def _live() -> None:
    wdg = ListWidget(FSEntry("/etc/udev"))
    odev = PrinterDevice()
    wdg.render_to(odev)
    wdg.set_entity(wdg[1])
    wdg.render_to(odev)
