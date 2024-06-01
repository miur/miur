# pylint:disable=too-few-public-methods
from typing import Callable, Iterable, Protocol, TypeVar

# RQ: you can assign *any* object into `ListWidget to ·interpret· and ·explore· its ·derivatives·
#   NOTE: object's composition (structure) is also a derivative, COS object itself is always *opaque*

# IDEA: `Entity is *always* a cached proxy representation of the real object (which may not even exist)
#   or a virtual aggregation of objects
#   COS: we need to store somewhere all pieces of cached dashboard state/data to draw

_Tx_co = TypeVar("_Tx_co", covariant=True)


# RENAME? more precicse =ListWidgetRepresentable
class Representable(Protocol):
    @property
    def name(self) -> str: ...
    # def explore(self) -> Iterable[_Tx_co]: ...


# RENAME? FSEntity, FSEntityAdapter, Path2Entity, InterpretAsFSEntity,
#   Mappable, Dashboard, WidgetLayout, VisibleFields
class FSEntry(Representable):
    def __init__(self, path: str) -> None:
        self._x = path

    # TEMP?(unless API-shortcut): should be accessible by explored GetNameAction
    # IDEA: always return repr(_x) first, and then await for async `Transmission of proper .name
    @property
    def name(self) -> str:
        return repr(self._x)

    # TEMP?API: discover this .method and manually wrap into `Sfn to represent in `Widget
    # SPLIT? .api() and .exec(), and then DFL explore= one of them
    #   i.e. `Transmission shouldn't start producing and caching any data until .exec() was called
    #   MAYBE: .exec() is also a *short-circuit*, as we may get ent.api().exec() instead
    #     ~~ anyway, at some point we should do actual .exec() inof recursively listing .api_of_api_of_exec()
    def explore(self) -> Iterable["FSEntry"]:
        # NOTE: wrap into specific `Adapter|Entity immediately here
        #  COS Widget expects to access e.g. .size for all available elements in list
        #  ALT:FUT: rfc code to allow *some* homogeneous entries behind FSAdapter to reduce RAM consumption
        with __import__("os").scandir(self._x) as it:
            return [FSEntry(e.path) for e in it]


class Sfn(Representable):
    def __init__(self, sfn: Callable[[], list[Representable]]) -> None:
        self._sfn = sfn

    @property
    def name(self) -> str:
        return repr(self._sfn)

    def __call__(self) -> list[Representable]:
        return self._sfn()


# NOTE: `BoundActionEntity is actually the `Widget itself with all choices done
#   ~~ COS if MillerWidget needs x3 _lst -- it will bleed into BoundActionEntity
class ListWidget:
    # SEP? InteractionModel[+Workflow] | {class DisplayableContext}
    #   COS: smth ought to decide what to show and how to .advance whatever shown
    _ent: Representable
    _act: Callable[[], list[Representable]]
    # NOTE: for `Transmission or `AsyncListing we won't even need _ent or _act,
    #   as they will be closured into _lst itself (o/w you won't be able to "refresh" _lst)
    #   [_] IDEA: call `Transmission as `Link [bw filter-entities] or `Stream
    _lst: list[Representable]
    # _ctx: self

    # RENAME? focus()
    def set_entity(self, ent: Representable) -> None:
        self._ent = ent
        # SEP: {class NaviHeuristics} (then we don't need dedicated .api OR .exec at all)
        if callable(ent):
            self._act = lambda: ent()  # pylint:disable=unnecessary-lambda
        else:
            self._act = lambda: [Sfn(getattr(ent, k)) for k in dir(ent) if k == 'explore']
        self._lst = self._act()

    # RENAME? actualize
    def pick(self, i: int) -> None:
        self.set_entity(self._lst[i])

    def render(self) -> None:
        print("  " * 0 + str(0) + ": " + self._ent.name)
        for i, x in enumerate(self._lst, start=1):
            print("  " * 1 + str(i) + ": " + x.name)


########################################
def _live() -> None:
    wdg = ListWidget()
    wdg.set_entity(FSEntry("/etc/udev"))
    wdg.render()
    wdg.pick(0)
    wdg.render()
