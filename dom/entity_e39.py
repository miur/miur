import os
from abc import ABCMeta, abstractmethod
from collections import deque
from pathlib import Path
from typing import Any, Callable, Iterable, Protocol, Self, Sequence

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


# FIXED:RENAME: DirView -> AsyncListingView
#   * we shouldn't cache both DirEntity and generic Listing with STD list-trfm API in single DirView
class AsyncListingView(Entity):
    def __init__(self, act: Action) -> None:
        self._act = act
        # lst = self._act()
        self._cache: deque[Entity] = deque()
        self.sorted: bool | Callable[[Entity], SupportsLessThan] | None = False
        self.reversed = False

    @property
    def name(self) -> str:
        return self._act.name + ".Listing"

    # THINK: maybe cache transformed results too?
    #   NEED: propagate changes/events in underlying FS till transformed results
    #     BUT: only if those changes are reflected in xfm, otherwise ignore them
    # DISABLED:WAIT: !python>=3.12
    # def _transform[Ts: Iterable[Entity]](self, xs: Ts) -> Ts:
    def _transform(self, xs: Sequence[Entity]) -> Iterable[Entity]:
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
        if self.reversed:
            return reversed(xs)
        return xs

    def __iter__(self) -> Iterable[Entity]:
        """[Sync.API] Return all currently present (cached) items."""
        # IDEA: allow `Cache to contain only «args» inof whole Entity,
        #    and create actual entries only when yielding items
        #  e.g. store only "str(Path)" inof InodeEntity for each item in DirListing
        #  COS: &why Reduce RAM consumption and CPU latency on unnecessary Entity creation
        #  BUT: assuming that View will be cached in Widget, we won't benefit much
        #  BAD: listing obtained twice will yield two different instances of `Entity
        return self._transform(self._cache)

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

    def __call__(self, *_args: P.args, **_kwds: P.kwargs) -> AsyncListingView:
        # FIXME: should be stateful [Async.API]
        with os.scandir(self._ent._path) as it:
            # THINK: construct new `*Entity directly here (to include ctx)
            #        or keep str() until later API -- to reduce memory consumption
            # return [e.name for e in it]
            # for e in it:
            #     yield (DirEntity if x.is_dir() else FileEntity)(e.path)
            return [e.name for e in it]


class PrinterDevice:
    def add_line(self, x: int, y: int, s: str) -> None:
        print("  " * x + str(y) + ": " + s)


# RENAME? Widget -> Presenter/Adapter
# TODO:(inheritance hierarchy): DirWidget(GenericEntityWidget(Entity))
class DirWidget:
    # _ent: DirEntity
    # _lst: AsyncListingView

    def __init__(self, ent: DirEntity | None = None) -> None:
        # self._show = "LsInodes"  # = «default action name»
        if ent:
            self.set_entity(ent)

    def set_entity(self, ent: DirEntity) -> None:
        # WARN: should probably be DirCachedProxy to cache fetched and generated results
        self._ent = ent
        # act = ent.LsinodesAction
        # view = AsyncListingView(act)
        ## DISABLED: It's too early for «short-circuiting»
        # self._lst = self._ent[self._show]()

    def render_to(self, odev: PrinterDevice) -> None:
        # IDEA:(sparse API): use ERROR placeholders if Entity doesn't have some of DirEntity methods
        odev.add_line(0, 0, self._ent.name)
        for i, ent in enumerate(self._lst, start=1):
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
