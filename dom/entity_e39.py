import os
from abc import ABCMeta, abstractmethod
from collections import deque
from pathlib import Path
from typing import Any, Callable, Iterable, Iterator, Protocol, Self, Sequence, overload

from typing_extensions import ParamSpec

# pylint:disable=too-few-public-methods

P = ParamSpec("P")


class SupportsLessThan(Protocol):
    def __lt__(self, __other: Any) -> bool:
        ...


### ARCH


class Entity(metaclass=ABCMeta):
    # default = "actionlist"

    # ALT: uid()  // = and avoid duplicate names
    # ALT: __str__()
    @property
    @abstractmethod
    def name(self) -> str:
        raise NotImplementedError

    def __lt__(self, other: Self) -> bool:
        return self.name < other.name


class Action(Entity):
    # default = "execute"

    # BET? make proper name e.g. exec()
    #   >> then in FUT I can switch default method filling results for `Widget.Area
    @abstractmethod
    def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> Any:
        raise NotImplementedError


# class ShowAllActions(Action):
#     def __call__(self, wdg: DirWidget) -> None:
#         wdg.set_entity(ent)  # OR wdg.ent


class InodeEntity(Entity):
    def __init__(self, path: str | Path) -> None:
        self._path = Path(path)

    @property
    def name(self) -> str:
        return self._path.as_posix()


class FileEntity(InodeEntity):
    def get_text(self) -> str:
        with open(self._path, "r", encoding="utf-8") as f:
            return f.readline()


class DirEntity(InodeEntity):
    # def get_actions(self) -> Iterable[Action]:
    #     yield ShowActionEntity("FSListWidget", "FSView", "get_listing")
    #     yield ShowActionEntity("VirtualListWidget", "EntityView", "get_actions")
    #     pass

    def _actions(self) -> Iterable[Action]:
        # CHG: don't generate new ones each time, simply traverse over existing CachedListingAction
        # THINK:DECI: early binding in __init__(), later in __getitem__, OR defer until Listing/Widget ?
        yield LsinodesAction(self)

    def __iter__(self) -> Iterable[Action]:
        return self._actions()

    # BET?(Any): use proper methods to typecheck in/out args inof type-erasure
    # def __getitem__(self, k: str) -> Any: if k == "name": return ...
    def __getitem__(self, k: str) -> Action:
        for act in self._actions():  # BAD:PERF
            if act.name == "LsInodes":
                return act
        raise NotImplementedError


# FIXED:RENAME: DirView -> AsyncCachedListing
#   * we shouldn't cache both DirEntity and generic Listing with STD list-trfm API in single DirView
class AsyncCachedListing(Entity):
    def __init__(self, act: Action) -> None:
        self._act = act
        # lst = self._act()
        # NOTE:(cache): is SparseCache which we splice() to replace placeholders "[...]/<loading>"
        self._cache: deque[Entity] = deque()

    @property
    def name(self) -> str:
        return self._act.name + ".Listing"

    # def prefetch(beg:int, end:int):
    #     """[Async.API] Clear cached items. NOTE: re-fetching should be initiated separately."""
    # def clear():
    #     """Clear cached items. NOTE: re-fetching should be initiated separately."""
    # def wait_done(timeout=None):
    #     """Block until *requested* ranges were fetched."""
    # def wait_any_event(timeout=None):
    #     """Block until any new event received e.g. listing state update."""
    # def wait_accumulated(timespan):
    #     """Block for exactly dt seconds to get efficient batch xfr from remote."""


class LsinodesAction(Action):
    name = "ListDir/Contents"

    def __init__(self, ent: DirEntity) -> None:
        self._ent = ent

    def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> AsyncCachedListing:
        # FIXME: should be stateful [Async.API]
        with os.scandir(self._ent._path) as it:
            # THINK: construct new `*Entity directly here (to include ctx)
            #        or keep str() until later API -- to reduce memory consumption
            # return [e.name for e in it]
            # for e in it:
            #     yield (DirEntity if x.is_dir() else FileEntity)(e.path)
            return [e.name for e in it]


class Transformer:
    # RENAME:(action-like): sortby, reverse
    sorted: bool | Callable[[Entity], SupportsLessThan] | None
    reversed: bool
    # - clip (head/tail/mid)
    # - filter
    # - flatten
    # - groupby
    # - annotate/enrich

    def __init__(self) -> None:
        self.sorted = False
        self.reversed = False

    # THINK: maybe cache transformed results too?
    #   NEED: propagate changes/events in underlying FS till transformed results
    #     BUT: only if those changes are reflected in xfm, otherwise ignore them
    # DISABLED:WAIT: !python>=3.12
    # def _transform[Ts: Iterable[Entity]](self, xs: Ts) -> Ts:
    def __call__(self, xs: Sequence[Entity]) -> Iterable[Entity]:
        """[View.API] Apply stateful transformation."""
        if self.sorted:
            fkey = None if isinstance(self.sorted, bool) else self.sorted
            return sorted(xs, key=fkey, reverse=self.reversed)
        # ALT:MAYBE: directly allow applying reversed(AsyncListing[ReversibleSequence])
        #   BUT: in that case I will still be forced to keep ViewState somewhere
        #     e.g. to pick when to apply reversed() and when not
        #   BET:SEP: `Transform to independent class, and allow reversed(AsyncListing)
        #     NICE: we can optimize underlying proto to start requesting items from back
        #     SEE: my older attempts to create Transform cls in #miur and #pa3arch
        #   WARN: `TransformedView should definitely be `Cached
        #     << COS sorted() is very slow, when we need to redraw `Widget many times
        #     BUT:ALSO: order/filter ops should be very fast
        #      >> so underlying `Listing should be `Cached too
        if self.reversed:
            return reversed(xs)
        return xs

    # NOTE: to be able to mutate CachedXfm in-place
    def apply_to(self, lst: list[Entity]) -> None:
        if self.sorted:
            fkey = None if isinstance(self.sorted, bool) else self.sorted
            lst.sort(key=fkey, reverse=self.reversed)
        if self.reversed:
            lst.reverse()


# RENAME? [Shallow]CachedXfm[=Transformer]
class CachedXfm:
    def __init__(self, als: AsyncCachedListing, tfmr: Transformer) -> None:
        self._als = als
        self._tfmr = tfmr
        # self._anno = Annotate(PersistentDB)  # = separate enricher
        self._cache = list(self._tfmr(self._als))

        # TODO: repeat on each update() of als ALSO update() on tfmr .fields change
        # FUT:ENH: make a protocol to update incrementally only whatever changed
        # self._tfmr.apply_to(self._cache)

    def __iter__(self) -> Iterator[Entity]:
        """[Sync.API] Return all currently present (cached) items."""
        # IDEA: allow `Cache to contain only «args» inof whole Entity,
        #    and create actual entries only when yielding items
        #  e.g. store only "str(Path)" inof InodeEntity for each item in DirListing
        #  COS: &why Reduce RAM consumption and CPU latency on unnecessary Entity creation
        #  BUT: assuming that View will be cached in Widget, we won't benefit much
        #  BAD: listing obtained twice will yield two different instances of `Entity
        return iter(self._cache)

    @overload
    def __getitem__(self, kx: slice) -> list[Entity]:
        ...

    @overload
    def __getitem__(self, kx: int) -> Entity:
        ...

    def __getitem__(self, kx: slice | int) -> list[Entity] | Entity:
        if isinstance(kx, (slice, int)):
            return self._cache[kx]
        # NICE: get list of items inof single one
        if isinstance(kx, tuple):
            return [self._cache[i] for i in kx]
        return NotImplementedError


# [_] TODO:RFC:USE:(python=3.12): View[T] ; CachedXfm[T] ; Entity → T
class View:
    _beg: int
    _num: int

    # ALT:SEE:(3-way):IMPL: just.reelf.dom.AddrRange(beg,sz,end)
    def __init__(self, xfm: CachedXfm, beg: int = 0, num: int = 0) -> None:
        self._xfm = xfm
        assert all(isinstance(x, int) for x in (beg, num))
        self.set_span(beg, num)

    def set_span(self, beg: int | None = None, num: int | None = None) -> None:
        if beg is not None:
            self._beg = beg
        if num is not None:
            self._num = num

    def __iter__(self) -> Iterable[Entity]:
        return self._xfm[self._beg : self._beg + self._num]


class PrinterDevice:
    def add_line(self, x: int, y: int, s: str) -> None:
        print("  " * x + str(y) + ": " + s)


# RENAME? Widget -> Presenter/Adapter
# TODO:(inheritance hierarchy): DirWidget(GenericEntityWidget(Entity))
class DirWidget:
    _ent: DirEntity
    _vw: AsyncCachedListing

    def __init__(self, ent: DirEntity | None = None) -> None:
        # self._show = "LsInodes"  # = «default action name»
        self._tfmr = Transformer()
        if ent:
            self.set_entity(ent)

    def set_entity(self, ent: DirEntity) -> None:
        # WARN: should probably be DirCachedProxy to cache fetched and generated results
        self._ent = ent
        # act = ent.LsinodesAction
        # view = AsyncCachedListing(act)

        ## NOTE: we imeediately do {Action.exec()->Listing} here in `Widget
        #   so it may seem tempting to combine Entity+Listing
        #    BUT(in general): we don't exec() any item in _lst itself,
        #     until it's passed to some `Widget or `Script,
        #     so separation of concepts is justified
        ## DISABLED: It's too early for «short-circuiting»
        # self._lst = self._ent[self._show]()
        # [_] WARN: when you re-assign _lst -- it shouldn't be destroyed
        #   >> WF: so you could switch back-n-forth previous assigned lists in same widget
        #   !! i.e. they should be in `CachePool first, and only handle assigned to self._lst
        #   ?? should I wrap all of them into ProxyToPool() to give back existing instance OR make a new
        #     COS when it's deleted from Pool to fit memory limits -- we need to know and drop or recreate
        self._view = View(CachedXfm(AsyncCachedListing(self._ent.ShowAllActions()), self._tfmr))

    def render_to(self, odev: PrinterDevice) -> None:
        # IDEA:(sparse API): use ERROR placeholders if Entity doesn't have some of DirEntity methods
        odev.add_line(0, 0, self._ent.name)
        for i, ent in enumerate(self._view, start=1):
            odev.add_line(1, i, ent)

    def handle_keypress(self, key: str) -> None:
        if key == "l":  # vim-right (to open directory)
            # TODO: switch to next Entity (to test reassign on Action.exec())
            # self.set_entity(ent_under_cursor)
            print(key)
        elif key == "o":  # =ordering
            self._lst.reversed = not self._lst.reversed
            print(key)
        else:
            raise NotImplementedError


def _live() -> None:
    dtmp = Path("/t")  # =/tmp/testmiur
    dtmp.mkdir(exist_ok=True)
    for c in "abc":
        dtmp.joinpath(c).touch(exist_ok=True)

    wdg = DirWidget(DirEntity("/t"))

    odev = PrinterDevice()
    wdg.render_to(odev)

    # wdg.handle_keypress("o")
    # wdg.render_to(odev)
    # wdg.handle_keypress("l")
    # wdg.render_to(odev)
