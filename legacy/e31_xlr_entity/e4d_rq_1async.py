# pylint:disable=too-few-public-methods
import time
from threading import Event
from concurrent.futures import Executor, Future, ThreadPoolExecutor
from typing import (  # Iterator, overload,
    Any,
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


########################################
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


class Sfn(Representable, Generic[_Tx_co]):
    def __init__(self, sfn: Callable[[Any], ListCachingProxy[_Tx_co]]) -> None:
        self._sfn = sfn

    @property
    def name(self) -> str:
        return repr(self._sfn)

    def __call__(self, ctx: Any) -> ListCachingProxy[_Tx_co]:
        return self._sfn(ctx)


########################################
WaitingPlaceholder = NewType("WaitingPlaceholder", Future[Any])
RequestPlaceholder = NewType("RequestPlaceholder", object)


class ProxyEvent:
    pass


# RENAME? ResponseValue
class UpdatedValue(Generic[T], ProxyEvent):
    def __init__(self, value: T) -> None:
        self._value = value

    @property
    def value(self) -> T:
        return self._value


# NOTE: next EVO is DictCachingProxy (for dashboards), as AsyncList is much more complex
# TODO: parametrize by `Evaluator
#   :: pass _fn to `ThreadPool for deferred calcs
#     OR eval _fn in-place to send request through `RPCFacade for *delegated* calcs
#   BET?TRY: hide `Evaluator in _fn and always eval
# RENAME? `AsyncFuture
class ValueCachingProxy(Generic[T]):
    # MAYBE:CHG: fn -> smth to send msg/req to and wait for response
    #   BUT: we still need smth (thread/queue) to WAIT for response
    def __init__(
        self, fn_req: Callable[[Callable[[ProxyEvent], None]], Future[T]]
    ) -> None:
        # TODO: need to register proxy's incoming queue for notifications
        self._fn = fn_req
        # ALT: return placeholder (DECI: by spec)
        # NOTE: intentionally don't assign None to avoid dealing with Optional[T] where T is expected
        self._cache: T
        # MOVE: to FSMMixin
        # MAYBE: I don't even need traditional "Future" for my purposes
        self._state: Future[T] | bool = False
        self._subscribers: list[Callable[[UpdatedValue[T]], None]] = []

    # MAYBE:NEED: comm_chan_id -- to distinguish earlier and newer async comm sessions
    #   BUT: are two simultaneous sessions even make sense here ?
    def _listener(self, ev: ProxyEvent) -> None:
        if isinstance(ev, UpdatedValue):
            self._cache = ev.value
            self._state = True
            for fsub in self._subscribers:
                fsub(UpdatedValue(self._cache))

    # OR: return awaitable object (blocking with timeout)
    def request_async(self, fsub: Callable[[UpdatedValue[T]], None]) -> None:
        # BAD: should only request eval, but not calc it immediately in-place here
        # MAYBE: return different placeholder comparing to before to indicate ongoing processing
        # [_] WARN:NEED: mutex
        if self._state is False:
            # NOTE: _fn should remember listener to return both oneshot and multipart responses
            self._state = self._fn(self._listener)
            # OR:(for regular `Futures): self._state.add_done_callback(lambda fut: self._listener(fut.result()))
        # BAD? additional responsibility in this function
        #   ALSO: self._fsub may be not available in a place where you are calling .request_async()
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
            case Future():
                return WaitingPlaceholder(self._state)
            case False:
                return RequestPlaceholder(self.request_async)
            case _:
                raise RuntimeError("Unsupported state", self._state)


########################################
class Counter:
    def __init__(self, flag_running: Event) -> None:
        self._running = flag_running
        self._i = 0
        # RENAME? connections, awaiters, links, nodes, sinks
        self._subscribers: list[Callable[[UpdatedValue[int]], None]] = []

    # NOTE: return handle to be able to unsubscribe() in .dtor
    def subscribe(self, fsub: Callable[[UpdatedValue[int]], None]) -> int:
        try:
            return self._subscribers.index(fsub)
        except ValueError:
            # BAD: all accesses should be protected by mutex
            self._subscribers.append(fsub)
            return len(self._subscribers) - 1

    def run(self) -> None:
        self._running.set()
        while self._running.is_set():
            self._i += 1
            self.notify_all()
            time.sleep(0.4)

    def notify_all(self) -> None:
        for fsub in self._subscribers:
            # MAYBE: transmit only _Pulse, and then pull() the value ?
            fsub(UpdatedValue(self._i))


########################################
# NOTE: `BoundActionEntity is actually the `Widget itself with all choices done
#   ~~ COS if MillerWidget needs x3 _lst -- it will bleed into BoundActionEntity
class ListWidget:
    ## VIZ: all of vars -- are `*Proxy used by `Dashboard(Layout)
    # SEP? InteractionModel[+Workflow] | {class DisplayableContext}
    #   COS: smth ought to decide what to show and how to .advance whatever shown
    _ent: Representable
    _act: Callable[[], ListCachingProxy[Representable]]
    # NOTE: for `Transmission or `AsyncListing we won't even need _ent or _act,
    #   as they will be closured into _lst itself (o/w you won't be able to "refresh" _lst)
    #   [_] IDEA: call `Transmission as `Link [bw filter-entities] or `Stream
    _lstpxy: ListCachingProxy[Representable]
    _valpxy: ValueCachingProxy[int]
    # _ctx: self

    def __init__(self, pool: Executor) -> None:
        self._pool = pool

    # RENAME? actualize
    def pick(self, i: int) -> None:
        self.set_entity(self._lstpxy[i])

    # RENAME? focus()
    def set_entity(self, ent: Representable) -> None:
        self._ent = ent
        # SEP: {class NaviHeuristics} (then we don't need dedicated .api OR .exec at all)
        if callable(ent):
            self._act = lambda: ent(ctx=self)
        else:
            self._act = lambda: ListCachingProxy(
                Sfn(getattr(ent, k)) for k in dir(ent) if k == "explore"
            )
        self._lstpxy = self._act()

        ## BAD: too complex fcalc() for user -- I need simpler evaluators
        ##   BUT: if I wish for something "chainable" -- is this complexity avoidable?
        # pylint:disable=unnecessary-lambda-assignment
        # : Callable[[Callable[[ProxyEvent], None]], None]
        fcalc = lambda cb: cb(UpdatedValue(len(self._lstpxy)))
        fpost = lambda cb: self._pool.submit(fcalc, cb)
        self._valpxy = ValueCachingProxy(fpost)
        self._valpxy.request_async(self._listen_events)

    # RENAME? parse_events
    def _listen_events(self, ev: UpdatedValue[int]) -> None:
        # THINK: do we really need this long mapping ? What flexibility does it stands for ?
        #   :: Future → _listen_cb() → Event[int[1]] → self._valpxy.get() → redraw_footer()
        #   MAYBE: directly call redraw_footer() in lambda after is updated
        #     BAD: we lose single common point to log/audit
        if ev.value == 1:
            self.redraw_footer()

    def redraw_footer(self) -> None:
        # e.g. curses_area_clean_and_redraw()
        s = str(v) if isinstance((v:=self._valpxy.get()), int) else repr(v)
        print("\r" + s)

    def render(self) -> None:
        print("  " * 0 + str(0) + ": " + self._ent.name)
        for i, x in enumerate(self._lstpxy, start=1):
            print("  " * 1 + str(i) + ": " + x.name)
        self.redraw_footer()


########################################
def _live() -> None:
    # HACK: cancel busy worker through the shared flag
    flag_running = Event()
    cnt = Counter(flag_running)

    with ThreadPoolExecutor(max_workers=2) as pool:
        # [_] TODO: lazy submittance by ListWidget "when shown for the first time"
        _fut1 = pool.submit(cnt.run)
        wdg = ListWidget(pool)
        wdg.set_entity(FSEntry("/etc/udev"))
        # BAD: should be "more automatic", BUT:THINK: where to do that?
        cnt.subscribe(lambda i: wdg._valpxy._listener(UpdatedValue(i)))
        wdg.render()
        wdg.pick(0)
        wdg.render()
        time.sleep(3)
        flag_running.clear()
