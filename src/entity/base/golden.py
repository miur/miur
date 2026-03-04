from collections.abc import Iterable
from typing import TYPE_CHECKING, Any, cast, override

from .interp import InterpretableImpl


# RENAME? Indivisible AtomicError HaltExploration Leaf SolidBox
class StopExploration(Exception):
    pass


# OR=GoldenProto[col]
type Entity = Golden[Any]  # pyright: ignore[reportExplicitAny]
type Entities = Iterable[Entity]


# RENAME? `Entity `Node `Discoverable
class Golden[T](InterpretableImpl):
    __slots__: tuple[str, ...] = ("_x", "_parent")
    _x: T
    _parent: Entity

    def __init__(self, x: T, parent: Entity, /) -> None:
        self._x = x
        self._parent = parent
        # self._parent: Golden[Any] = parent  # type: ignore[assignment]

    # NICE: as we have a separate .name field, we can *dynamically* augment regular filenames
    # ex~:
    #   ~ skip date-prefix (to sort by body)
    #   ~ substitute my xts3 to isodate (BET: .repr_as())
    #   ~ insert full path into name -- for flattened folders or symlinks
    #   ~ smart-compress long textlines to use them everywhere as unique names
    # ALT:BET? do it inside "item visualization", which would be both more logical and practical
    #   i.e. we may visualize both augmented and shadowed original names at the same time
    # FUT:PERF:CMP: use @cached_property vs @property+@lru_cache(1) vs .update(self._name) method
    @property
    def name(self) -> str:
        return str(self._x)

    ## SUM: merged into Golden, standalone Protocol won't work as parent should *always* return GoldenAny
    # NOTE: directly point to `Entity which introspected it or `Action which produced it
    #   => and then you can access cached EntityView._vlst and _wdg only through _pool
    #   i.e. individual `Entity will lose the ability to directly access associated _wdg
    #     BUT: we won't need to eventually use "weak_ref" for _pv to release pool memory
    #  ALT: MAYBE: make tight coupling bw `EntityView -> `Entity(pview=self)
    #    i.e. immeditely make it exist for each item like {EntityView(): RootNode(self)}
    # WARN! here parent=Action(lambda: explore()) inof prev `Entity
    #   => USE:CASE: able to jump to unrelated node, press <Back> and get to actions which produced it
    # FAIL: we can't link `Entity to its `Action, so we lose "actions" in `Loci URL
    #     orse we would need: Action.explore() { Entity.explore(parent=self) }
    @property
    def parent(self) -> Entity:
        # assert self._parent != self
        return self._parent

    # REMOVE? -> construct `Loci on demand through external means
    # @cached_property
    @property
    def loci(self) -> str:
        assert self._parent is not self
        # BAD:PERF: recursive
        # FAIL:TEMP: "self.name" -> "self._x.selector_for_interpreter"
        return self._parent.loci + "/" + self.name

    ## SUM: merged into Golden -- standalone Protocol has no sense for mixed Golden containers
    # BAD: names can be non-unique; BET? disallow or keep by order added ?
    def __lt__(self, other: Entity) -> bool:
        # ALT: comparing .loci may be unreliable w/o miur:// URL schema
        return self.name < other.name

    @override
    def __repr__(self) -> str:
        loci = self.loci
        if not loci.endswith(nm := self.name):
            loci = nm + loci
        # if loci.startswith("/"):
        #     return f"`{loci}"
        return f"{type(self).__name__}({loci})"

    ## ARCH:
    #  * on ERROR -> return [`ErrorEntry], mixed with regular entries
    #    >> if whole list ~can't be read~ -- it will result in empty list with error
    #    COS: we may get multiple errors, for e.g. unreadable elements in the list
    #  * on empty list -> return [], and interpret it based on `*Entry itself
    #    e.g. to make different messages for empty folder and empty file
    #  * if entry is atomic -> return "None", and again interpret it based on `*Entry
    #    COS: behavior of HaltEntry is NOT inherent and depends on how we decide to interpret it
    #    ALT:BET? remove the method itself and use getattr() to verify its presence
    # MAYBE: make default entity `Atomic, so exploration would return nothing or error ?
    def explore(self) -> Entities:
        # NOTE: it's reasonable to raise inof using @abc.abstractmethod
        #   * rarely we may have atomic leafs, but for most it's "NOT YET IMPLEMENTED"
        #   * we will use try-catch around .explore() anyway -- to catch errors
        raise NotImplementedError("TBD: not yet implemented")


# pyright: reportUnusedFunction=false
if TYPE_CHECKING:
    from .traits import GoldenProtocol

    ## FIXED:ERR: `FSEntry [too-many-ancestors] Too many ancestors (8/7)
    # NOTE: validate that the class constructor matches the protocol requirements
    # REF: Is there a way to tell mypy to check a class implements a Protocol without inheriting from it? ⌇⡧⢟⠦⡴
    #   https://github.com/python/mypy/issues/8235
    # WARN: we rely on !mypy to verify this (inof runtime checks)
    # FAIL: it's supposed to catch when we are missing methods...
    # EXPL: Why none of your checks catch the "parent -> Self" mismatch
    #   Because Golden doesn't inherit from GoldenProtocol, the checker verifies structural compatibility lazily
    #   — only at actual assignment. And the parent property in your LocatableImpl returns Golden[Any], while
    #   Locatable protocol expects Self. But when checking Golden[complex] against GoldenProtocol, the checker
    #   substitutes Self = Golden[complex] and then checks if Golden[Any] is assignable to Golden[complex]...
    #   which passes because Any wins.
    GoldenDummy = Golden[complex]  # OR=None|Literal["dummy"]
    _type: type[GoldenProtocol] = GoldenDummy
    _inst = GoldenDummy(complex(), cast(GoldenDummy, object))
    # from typing import reveal_type
    # reveal_type(GoldenDummy.parent)
    # reveal_type(GoldenDummy.explore)
    # reveal_type(GoldenDummy.interp_as)

    # Verify Golden[X] is assignable to GoldenProtocol for a concrete X
    # Use a concrete non-Any type that has no special assignability rules
    _proto: GoldenProtocol = cast(GoldenDummy, object)
    _golden: GoldenDummy = _proto  # type: ignore[assignment]  # reverse check
    _back: GoldenProtocol = _golden  # forward check — this is the useful one

    # class GoldenDummy(Golden[None]):
    #     def __init__(self) -> None:
    #         super().__init__(None, self)
    #     @override
    #     def explore(self) -> Entities:
    #         raise NotImplementedError
    # inst: GoldenProtocol = GoldenDummy()

    def _typecheck_protocols() -> None:
        # fmt:off
        # pylint:disable=multiple-statements
        from . import traits as TR
        def _check_representable(x: GoldenDummy) -> TR.Representable: return x
        def _check_sortable(x: GoldenDummy) -> TR.Sortable: return x
        def _check_backtrackable(x: GoldenDummy) -> TR.Backtrackable: return x
        def _check_locatable(x: GoldenDummy) -> TR.Locatable: return x
        def _check_explorable(x: GoldenDummy) -> TR.Explorable: return x
        def _check_interpretable(x: GoldenDummy) -> TR.Interpretable: return x
        def _check_golden(x: GoldenDummy) -> TR.GoldenProtocol: return x
        # fmt:on

        ## ALT:
        # from typing import assert_type
        # assert_type(cast(Golden[int], None), GoldenProtocol)
