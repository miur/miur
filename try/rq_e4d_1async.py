# pylint:disable=too-few-public-methods
from typing import (  # Any, Iterator, overload,
    Callable,
    Generic,
    Iterable,
    NewType,
    Protocol,
    TypeVar,
)

# RQ: you can assign *any* object into `ListWidget to ·interpret· and ·explore· its ·derivatives·
#   NOTE: object's composition (structure) is also a derivative, COS object itself is always *opaque*

# IDEA: `Entity is *always* a cached proxy representation of the real object (which may not even exist)
#   or a virtual aggregation of objects
#   COS: we need to store somewhere all pieces of cached dashboard state/data to draw

T = TypeVar("T")
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


# class PublicCachedListAPI(Protocol[T]):
#     def __iter__(self) -> Iterator[T]: ...
#
#     def __getitem__(self, kx: slice | int, /) -> list[T] | T: ...
#
# # NOTE: only nested "self._cache" should have these API/setter
# class PrivateCachedListAPI(PublicCachedListAPI[T]):
#     pass
#
# class PublicCachedListGetterMixin(PublicCachedListAPI[T]):
#     _cache: PrivateCachedListAPI[T]


# class PublicCachedListGetterMixin(Generic[T]):
#     _cache: list[T]
#
#     def __iter__(self) -> Iterator[T]:
#         return iter(self._cache)
#
#     @overload
#     def __getitem__(self, kx: int, /) -> T: ...
#
#     @overload
#     def __getitem__(self, kx: slice, /) -> list[T]: ...
#
#     @overload
#     def __getitem__(self, kx: slice | tuple[int]) -> list[T]: ...
#
#     def __getitem__(self, kx: slice | int | tuple[int], /) -> list[T] | T:
#         if isinstance(kx, (slice, int)):
#             return self._cache[kx]
#         if isinstance(kx, tuple):  # NICE: get list of items ex~: a[(1,5,8)]
#             return [self._cache[int(i)] for i in kx]
#         return NotImplementedError
#
#
# # ARCH: Transmission[Proto/API] + Cache[State[List]]
# #   ALSO: parametrize by Fetching(Acquisition)Strategy and RetentionPolicy
# class ListCachingProxy(Generic[T], PublicCachedListGetterMixin[T]):
#     def __init__(self, xs: Iterable[T]) -> None:
#         self._cache: list[T] = list(xs)


class ListCachingProxy(list[T]):
    pass


FutureHandle = NewType("FutureHandle", int)
WaitingPlaceholder = NewType("WaitingPlaceholder", FutureHandle)
RequestPlaceholder = NewType("RequestPlaceholder", object)
ProxyEvent = NewType("ProxyEvent", object)


# NOTE: next EVO is DictCachingProxy (for dashboards), as AsyncList is much more complex
# TODO: parametrize by `Evaluator
#   :: pass _fn to `ThreadPool for deferred calcs
#     OR eval _fn in-place to send request through `RPCFacade for *delegated* calcs
#   BET?TRY: hide `Evaluator in _fn and always eval
class ValueCachingProxy(Generic[T]):
    # MAYBE:CHG: fn -> smth to send msg/req to and wait for response
    #   BUT: we still need smth (thread/queue) to WAIT for response
    def __init__(self, fn_req: Callable[[Callable[[ProxyEvent], None]], FutureHandle]) -> None:
        # TODO: need to register proxy's incoming queue for notifications
        self._fn = fn_req
        # ALT: return placeholder (DECI: by spec)
        self._cache: T | None = None
        # MOVE: to FSMMixin
        self._state: FutureHandle | bool = False
        self._subscribers: list[Callable[[ProxyEvent], None]] = []

    # MAYBE:NEED: comm_chan_id -- to distinguish earlier and newer async comm sessions
    #   BUT: are two simultaneous sessions even make sense here ?
    def _listener(self, ev: ProxyEvent) -> None:
        if ev.type == ResponseValue:
            self._cache = ev.value
            self._state = True
            for fsub in self._subscribers:
                fsub(UpdatedValue(self._cache))

    # OR: return awaitable object (blocking with timeout)
    def request_async(self, fsub: Callable[[ProxyEvent], None]) -> None:
        # BAD: should only request eval, but not calc it immediately in-place here
        # MAYBE: return different placeholder comparing to before to indicate ongoing processing
        # [_] WARN:NEED: mutex
        if self._state is False:
            # NOTE: _fn should remember listener to return both oneshot and multipart responses
            self._state = self._fn(self._listener)
        if fsub not in self._subscribers:
            self._subscribers.append(fsub)

    def get(self) -> T | WaitingPlaceholder | RequestPlaceholder:
        match self._state:
            case True:
                if self._cache is not None:
                    return self._cache
                raise RuntimeError(
                    "Broken invariant: cached value shouldn't be None"
                    " when FSM request state had transitioned to True"
                )
            case FutureHandle(_):
                return WaitingPlaceholder(self._state)
            case False:
                return RequestPlaceholder(self.request_async)


class Sfn(Representable, Generic[_Tx_co]):
    def __init__(self, sfn: Callable[[], ListCachingProxy[_Tx_co]]) -> None:
        self._sfn = sfn

    @property
    def name(self) -> str:
        return repr(self._sfn)

    def __call__(self) -> ListCachingProxy[_Tx_co]:
        return self._sfn()


# NOTE: `BoundActionEntity is actually the `Widget itself with all choices done
#   ~~ COS if MillerWidget needs x3 _lst -- it will bleed into BoundActionEntity
class ListWidget:
    # SEP? InteractionModel[+Workflow] | {class DisplayableContext}
    #   COS: smth ought to decide what to show and how to .advance whatever shown
    _ent: Representable
    _act: Callable[[], ListCachingProxy[Representable]]
    # NOTE: for `Transmission or `AsyncListing we won't even need _ent or _act,
    #   as they will be closured into _lst itself (o/w you won't be able to "refresh" _lst)
    #   [_] IDEA: call `Transmission as `Link [bw filter-entities] or `Stream
    _lstpxy: ListCachingProxy[Representable]
    # _ctx: self

    # RENAME? focus()
    def set_entity(self, ent: Representable) -> None:
        self._ent = ent
        # SEP: {class NaviHeuristics} (then we don't need dedicated .api OR .exec at all)
        if callable(ent):
            self._act = lambda: ent()  # pylint:disable=unnecessary-lambda
        else:
            self._act = lambda: ListCachingProxy(
                Sfn(getattr(ent, k)) for k in dir(ent) if k == "explore"
            )
        self._lstpxy = self._act()

    # RENAME? actualize
    def pick(self, i: int) -> None:
        self.set_entity(self._lstpxy[i])

    def render(self) -> None:
        print("  " * 0 + str(0) + ": " + self._ent.name)
        for i, x in enumerate(self._lstpxy, start=1):
            print("  " * 1 + str(i) + ": " + x.name)


########################################
def _live() -> None:
    wdg = ListWidget()
    wdg.set_entity(FSEntry("/etc/udev"))
    wdg.render()
    wdg.pick(0)
    wdg.render()
