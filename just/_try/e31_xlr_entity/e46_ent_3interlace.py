from abc import ABCMeta, abstractmethod
from functools import singledispatch
from inspect import signature
from pathlib import Path
from typing import (
    Annotated,
    Any,
    Callable,
    Iterable,
    Iterator,
    Protocol,
    Sequence,
    TypeVar,
    runtime_checkable,
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
        # print(f"{x!r}")

    def __repr__(self) -> str:
        ot = type(self).__name__  # __qualname__
        it = type(self._x).__name__
        sv = repr(self._x)
        if not sv.startswith(it):
            sv = f"{it}({sv})"
        return f"{ot}({sv})"  # f'{ot}<{sv}>'

    def __eq__(self, other: Any) -> bool:
        # isinstance(other, Entity) and
        if type(self) is type(other):
            return bool(self._x == other._x)
        return NotImplemented

    # NOTE: = .apply_immediately() inof .bind() for deferred (like Future)
    # @abstractmethod
    def __mul__(self, sfn: Callable[[_Tx_co], _REntity_co], /) -> _REntity_co:
        return sfn(self._x)

    @property
    def name(self) -> str:
        return repr(self._x)


########################################
# EVO: CachedAsyncListingProxy with non-"list" IMPL
class CachedListing(Entity[Sequence[T]]):
    def __init__(self, xs: Iterable[T]) -> None:
        super().__init__(list(xs))

    def __iter__(self) -> Iterator[T]:
        return iter(self._x)


########################################
class Applicable(Protocol[_Tx_contra]):
    def __call__(self, x: _Tx_contra, /) -> Composable[Any]: ...


_g_actions: dict[type, list[type]] = {}


# Action = Entity[Context] + SpecialFunction
class Action(
    Entity[Annotated[_Tx_co, "Context"]],
    Applicable[Annotated[_Tx_contra, "Inner"]],
):
    # ALT: recursively inspect Action.__subclasses__()
    #   ALT: walk through whole runtime code and inspect everything in existence
    def __init_subclass__(cls) -> None:
        super().__init_subclass__()
        ta = tuple(signature(cls.__call__).parameters.values())[1].annotation
        _g_actions.setdefault(ta, []).append(cls)

    @abstractmethod
    def __call__(self, x: _Tx_contra, /) -> Entity[Any]: ...


# RENAME? List[All|Available|Supported|Implemented|Accepted]Actions
class ListActions(Action[None, Any]):
    # BAD: we should have specific "ActionType" inof "type" here
    def __call__(self, x: Any, /) -> CachedListing[Action[None, type]]:
        """Look for actions bound to specific `*Entity instance or any of its base classes"""
        # [_] ALSO: should match CachedListing[Sequence[T]] onto Sequence[Entity] bound type
        hier = [x, *type(x).mro()]
        # BAD: how to *defer* creation of actions OR pass ctor(ctx) directly here
        #   BAD: ctor(ctx) may be different for dif Actions
        #   WARN: ListActions() should alway sreturn `Entity -- to be able to show them in `Widget
        #   MAYBE:IDEA: wrap each "Action class" into Entity[type], inof returning instantiated Actions
        return CachedListing(a(None) for t in hier for a in _g_actions.get(t, []))


class GetByIndex(Action[int, Sequence[Any]]):
    def __call__(self, xs: Sequence[_REntity_co]) -> _REntity_co:
        return xs[self._x]


# RENAME? OLD=BoundActionEntity NEW=«Explorable» (better than «Expandable»)
# class LiftedClosure(Entity[Callable[[_Tx_co], _REntity_co]]): ...
# [?] ALT:IDEA: directly store Action inof Callable
@runtime_checkable
class LiftedClosure(Protocol[_Tx_contra, T]):
    def __mul__(self, sfn: Callable[[Callable[[_Tx_contra], T]], T], /) -> T: ...


# RENAME? OLD=ExecAction,WrappedContext
class ExecWithContext(Action[_Tx_co, Callable[[_Tx_co], _REntity_co]]):
    def __call__(self, xfn: Callable[[_Tx_co], _REntity_co], /) -> _REntity_co:
        return xfn(self._x)


########################################

# AnyPath = Union[str, bytes, PathLike[str], PathLike[bytes]] | int(fd) | Path
## FAIL: PathLike NewType is stored as simple "str" inside Entity >> so no runtime introspection
# PathLike = NewType("PathLike", str)
## BET: totally opaque container type
#   -- used only to attach related Actions
# class PathLike(str):
#     pass
PathLike = Path


class FSEntry(Entity[PathLike]):
    # _x: Annotated[str, "Path"]
    def __init__(self, path: PathLike) -> None:
        # [_] TODO: use some type-frwk for runtime-checks?
        assert isinstance(path, PathLike)
        super().__init__(path)


# THINK: how to inherit from LiftedClosure ?
# ALT?
# class FSLifted(Entity[Action[None, Callable[[PathLike], CachedListing[FSEntry]]]]):
# class FSLifted(Entity[ExecWithContext[PathLike, CachedListing[FSEntry]]]):
class FSLifted(Entity[Callable[[PathLike], CachedListing[FSEntry]]]):
    pass



# [_] THINK: how to stop annotating *class itself* by actual types
class ListFSEntries(Action[None, PathLike]):
    def __call__(self, path: PathLike) -> CachedListing[FSEntry]:
        with __import__("os").scandir(path) as it:
            # WTF: why !mypy not forces to wrap into Path
            # return CachedListing(FSEntry(e.path) for e in it)
            return CachedListing(FSEntry(PathLike(e.path)) for e in it)


########################################
class Keyval(Entity[tuple[str, _Tx_co]]):
    def __init__(self, k: str, v: _Tx_co) -> None:
        super().__init__((k, v))

    @property
    def name(self) -> str:
        return self._x[0] + ": " + repr(self._x[1])


class ListFSStats(Action[None, PathLike]):
    def __call__(self, path: PathLike) -> CachedListing[Keyval[int]]:
        st = __import__("os").lstat(path)
        return CachedListing(
            Keyval(k, getattr(st, k)) for k in dir(st) if k.startswith("st_")
        )


########################################
@singledispatch
def default_action(ent: Entity[Any], ctx: Any) -> CachedListing[Any]:
    raise NotImplementedError


@default_action.register
def _(ent: FSEntry, ctx: None) -> CachedListing[FSEntry]:
    return ent * ListFSEntries(ctx)


# @default_action.register
# def _(ent: FSEntry, ctx: None) -> CachedListing[Keyval[int]]:
#     return ent * ListFSStats(ctx)

# @default_action.register
# def _(ent: FSEntry, ctx: None) -> CachedListing[Action[None, type]]:
#     return ent * ListFSStats(ctx)


########################################
class PrinterDevice:
    def add_line(self, x: int, y: int, s: str) -> None:
        print("  " * x + str(y) + ": " + s)


# NOTE: generalized viewer COS it may contain mix of different `*Entities
class ListWidget:
    _ent: FSEntry | FSLifted
    # NOTE: actions and entities can be mixed in some WF for better usability
    _lst: CachedListing[FSEntry | FSLifted]

    # _default_action: Mapping[Entity[Any], Action[Any, Any]] = {FSEntry: ListFSEntries}

    # def __init__(self, ent: FSEntry) -> None:
    #     self.set_entity(ent)

    def set_entity(self, ent: FSEntry | FSLifted) -> None:
        self._ent = ent
        # self._lst = ent * ListWidget._default_action.get(type(ent), ListActions)(None)
        # self._lst = default_action(ent, None)
        # FAIL: we need to get actions *already bound to each entry*,
        #   otherwise we can't pass both FSEntry and Action here
        if isinstance(ent, FSLifted):
            self._lst = ent * ExecWithContext(self)
        else:
            self._lst = ent * ListActions(None)

    def __getitem__(self, idx: int) -> FSEntry | FSLifted:  # TEMP:API
        return self._lst * GetByIndex(idx)

    def render_to(self, odev: PrinterDevice) -> None:
        odev.add_line(0, 0, self._ent.name)
        for i, ent in enumerate(self._lst, start=1):
            odev.add_line(1, i, ent.name)


########################################
def _live() -> None:
    en0 = FSEntry(PathLike("/etc/udev"))

    # print("---")
    # print(f"E0: {en0!r}")

    ac1 = ListFSEntries(None)
    ac2 = GetByIndex(1)
    # en1: FSEntry = ac2(ac1(en0))  # =if(ac*:Applicative)
    en1: FSEntry = en0 * ac1 * ac2
    _ls2 = en1 * ListActions(None) * GetByIndex(1)

    # ls1 = en0 * ac1
    # print(f"A1: {ac1!r}")
    # print(f"L1: {ls1!r}")
    # print(f"E1: {en1!r}")

    # print(f"{_g_actions=}\n")
    # print("_g_actions={" + "".join(f"\n  {k} = {v}" for k,v in _g_actions.items()) + "\n}\n")
    # print(f"{_g_actions[PathLike]=}\n")
    # print(en1 * ListActions(None))

    wdg = ListWidget()
    wdg.set_entity(en0)
    odev = PrinterDevice()
    wdg.render_to(odev)
    # assert en1 == wdg[1], (en1, wdg[1])
    # [_] BET: inof lambda -- store ClosureStruct and then call yourself, as lambda are always the same
    #   NICE: explorable ClosureStruct ++ no need to infer lambda type
    # bnd = FSLifted(lambda ctx, ent=en0: wdg[1](ctx)(ent))
    def _xfn(ctx: ListWidget) -> CachedListing[FSEntry]:
        return en0 * wdg[1](ctx)
    bnd = FSLifted(_xfn)
    wdg.set_entity(bnd)
    wdg.render_to(odev)
