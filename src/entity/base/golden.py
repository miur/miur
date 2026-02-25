from functools import cached_property
from typing import TYPE_CHECKING, Any, Callable, Iterable, Protocol, override

from .autodiscover import AutoRegistered
from .interp import InterpretableImpl
from .traits import GoldenStandartProtocol

type Entity = Golden[Any]
type Entities = Iterable[Entity]


# RENAME? Indivisible AtomicError HaltExploration Leaf SolidBox
class StopExploration(Exception):
    pass


# [SystemObject]Accessor/Proxy = .handle(AddressOfStorage) + BackendDataProviderSystem
#   ++ (LineBasedInterpreter+Selector) + StrTypeConverter
#   * UpdateProtocol | .update/invalidate function | event stream
#   * allows e.g. all FS ops
class Accessor(Protocol):
    # BAD: we can't pass "x:str" anymore
    # @property
    # def handle(self) -> Any: ...

    # RENAME? .get() or .read() | .getdata .readstr .get(type(str))
    @override
    def __str__(self) -> str: ...


# RENAME? @implements
def augment_with[T](*providers: type[Any]) -> Callable[[type[T]], type[T]]:
    def wrapper(cls: type[T]) -> type[T]:
        for provider in providers:
            for name, attr in provider.__dict__.items():
                if not name.startswith("__"):
                    setattr(cls, name, attr)
        return cls

    return wrapper


# RENAME? `Entity `Node `Discoverable
# ALT:(Protocol):NEED:BAD:PERF:(@runtime_checkable):COS:TEMP: to focus_on(match-case Golden())
# @augment_with(GoldenProtocol, RepresentableImpl, AddressableImpl, ExplorableImpl, InterpretableImpl)
@augment_with(InterpretableImpl)
class Golden[T](
    AutoRegistered, GoldenStandartProtocol
):  # (false-positive for protocols) pylint:disable=too-many-ancestors
    __slots__ = ()
    # __slots__ = ("_x", "__dict__")

    # ERR:(pylint=3.3.1):TRY: upgrade?
    # pylint:disable=undefined-variable
    def __init__(self, x: T, parent: Entity, /) -> None:
        self._x = x
        self._parent = parent

    ## ALT:(augment_with): delegate calls to "mixin" instances (=Composition=)
    # def __init__(self):
    #     self._handler = DataHandler()
    # def __getattr__(self, name):
    #     return getattr(self._handler, name)

    # NICE: as we have a separate .name field, we can *dynamically* augment regular filenames
    # ex~:
    #   ~ skip date-prefix (to sort by body)
    #   ~ substitute my xts3 to isodate
    #   ~ insert full path into name -- for flattened folders
    #   ~ smart-compress long textlines to use them everywhere as unique names
    # ALT:BET? do it inside "item visualization", which would be both more logical and practical
    #   i.e. we may visualize both augmented and shadowed original names at the same time
    @override
    @property
    def name(self) -> str:
        # FUT:PERF:CMP: use @cached_property vs @property+@lru_cache(1) vs .update(self._name) method
        return str(self._x)

    @override
    def explore(self) -> Entities:
        # NOTE: it's reasonable to raise inof using @abc.abstractmethod
        #   * rarely we may have atomic leafs, but for most it's "NOT YET IMPLEMENTED"
        #   * we will use try-catch around .explore() anyway -- to catch errors
        raise NotImplementedError("TBD: not yet implemented")

    # NOTE: directly point to `Entity which introspected it or `Action which produced it
    #   => and then you can access cached EntityView._vlst and _wdg only through _pool
    #   i.e. individual `Entity will lose the ability to directly access associated _wdg
    #     BUT: we won't need to eventually use "weak_ref" for _pv to release pool memory
    #  ALT: MAYBE: make tight coupling bw `EntityView -> `Entity(pview=self)
    #    i.e. immeditely make it exist for each item like {EntityView(): RootNode(self)}
    # WARN! here parent=Action(lambda: explore()) inof prev `Entity
    #   => USE:CASE: able to jump to unrelated node, press <Back> and get to actions which produced it
    #   [_] FAIL: we can't link `Entity to its `Action, so we lose "actions" in `Loci URL
    #     orse we would need: Action.explore() { Entity.explore(parent=self) }
    @override
    @property
    def parent(self) -> Entity:
        # assert self._parent != self
        return self._parent

    # REMOVE? -> construct `Loci on demand through external means
    @override
    @cached_property
    def loci(self) -> str:
        assert self._parent != self
        # BAD:PERF: recursive
        # FAIL:TEMP: "self.name" -> "self._x.selector_for_interpreter"
        return self._parent.loci + "/" + self.name

    # BAD: names can be non-unique; BET? disallow or keep by order added ?
    @override
    def __lt__(self, other: Entity) -> bool:
        return self.name < other.name

    @override
    def __repr__(self) -> str:
        loci = self.loci
        if not loci.endswith(nm := self.name):
            loci = nm + loci
        # if loci.startswith("/"):
        #     return f"`{loci}"
        return f"{type(self).__name__}({loci})"


if TYPE_CHECKING:
    # This will now PASS if all methods are present and FAIL if Golden is missing even one method.
    def verify_interface(_obj: GoldenStandartProtocol) -> None: ...

    inst = Golden[None](None, None)
    verify_interface(inst)  # Static check

    ## FIXED:ERR: `FSEntry [too-many-ancestors] Too many ancestors (8/7)
    # NOTE: validate that the class constructor matches the protocol requirements
    # REF: Is there a way to tell mypy to check a class implements a Protocol without inheriting from it? ⌇⡧⢟⠦⡴
    #   https://github.com/python/mypy/issues/8235
    # WARN: we rely on !mypy to verify this (inof runtime checks)
    # FAIL: it's supposed to catch when we are missing methods...
    _check: type[GoldenStandartProtocol] = Golden[Any]
