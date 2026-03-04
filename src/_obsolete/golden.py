from typing import TYPE_CHECKING, Any, cast, override

# from .interp import InterpretableImpl
from .traits import Entities, GoldenProtocol, Representable


type GoldenAny = Golden[Any]


class RepresentableImpl:
    __slots__ = ()
    _x: Any

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


# MOVE? merge-into Golden, no sense for standalone due to homohenicity of Entitites lists
class SortableImpl(RepresentableImpl):
    __slots__ = ()

    # BAD: names can be non-unique; BET? disallow or keep by order added ?
    #   ALT: comparing .loci may be unreliable w/o miur:// URL schema
    def __lt__(self, other: Representable) -> bool:
        return self.name < other.name


# MOVE? merge-into Golden, no sense for standalone due to return type
class BacktrackableImpl:
    __slots__ = ()
    _parent: GoldenAny

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
    def parent(self) -> GoldenAny:
        # assert self._parent != self
        return self._parent


class LocatableImpl(BacktrackableImpl, RepresentableImpl):
    __slots__ = ()

    # REMOVE? -> construct `Loci on demand through external means
    # @cached_property
    @property
    def loci(self) -> str:
        assert self._parent is not self
        # BAD:PERF: recursive
        # FAIL:TEMP: "self.name" -> "self._x.selector_for_interpreter"
        return self._parent.loci + "/" + self.name


## ARCH:
#  * on ERROR -> return [`ErrorEntry], mixed with regular entries
#    >> if whole list ~can't be read~ -- it will result in empty list with error
#    COS: we may get multiple errors, for e.g. unreadable elements in the list
#  * on empty list -> return [], and interpret it based on `*Entry itself
#    e.g. to make different messages for empty folder and empty file
#  * if entry is atomic -> return "None", and again interpret it based on `*Entry
#    COS: behavior of HaltEntry is NOT inherent and depends on how we decide to interpret it
#    ALT:BET? remove the method itself and use getattr() to verify its presence
class ExplorableImpl:
    __slots__ = ()

    # MAYBE: make default entity `Atomic, so exploration would return nothing or error ?
    def explore(self) -> Entities:
        # NOTE: it's reasonable to raise inof using @abc.abstractmethod
        #   * rarely we may have atomic leafs, but for most it's "NOT YET IMPLEMENTED"
        #   * we will use try-catch around .explore() anyway -- to catch errors
        raise NotImplementedError("TBD: not yet implemented")


# RENAME? `Entity `Node `Discoverable
class Golden[T](
    # InterpretableImpl,
    ExplorableImpl,
    LocatableImpl,
    BacktrackableImpl,
    SortableImpl,
    RepresentableImpl,
):  # pylint: disable=abstract-method
    __slots__ = ("_x", "_parent")

    def __init__(self, x: T, parent: GoldenAny, /) -> None:
        self._x = x
        self._parent = parent
        # self._parent: Golden[Any] = parent  # type: ignore[assignment]

    @override
    def __repr__(self) -> str:
        loci = self.loci
        if not loci.endswith(nm := self.name):
            loci = nm + loci
        # if loci.startswith("/"):
        #     return f"`{loci}"
        return f"{type(self).__name__}({loci})"


if TYPE_CHECKING:
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
    # from typing import reveal_type
    # reveal_type(GoldenDummy.parent)
    # reveal_type(GoldenDummy.explore)
    # reveal_type(GoldenDummy.interp_as)
    _type: type[GoldenProtocol] = GoldenDummy
    _inst = GoldenDummy(complex(), cast(GoldenDummy, None))

    # Verify Golden[X] is assignable to GoldenProtocol for a concrete X
    # Use a concrete non-Any type that has no special assignability rules
    _proto: GoldenProtocol = cast(GoldenDummy, None)
    _golden: GoldenDummy = _proto  # type: ignore[assignment]  # reverse check
    _back: GoldenProtocol = _golden  # forward check — this is the useful one

    # fmt:off
    # pylint:disable=multiple-statements
    from .traits import Explorable, Interpretable, Locatable, Sortable
    def _check_interpretable(x: GoldenDummy) -> Interpretable: return x
    def _check_explorable(x: GoldenDummy) -> Explorable: return x
    def _check_locatable(x: GoldenDummy) -> Locatable: return x
    def _check_representable(x: GoldenDummy) -> Representable: return x
    def _check_sortable(x: GoldenDummy) -> Sortable: return x
    def _verify_compat(x: GoldenDummy) -> GoldenProtocol: return x
    # fmt:on
    _check_interpretable(_inst)
    _check_explorable(_inst)
    _check_locatable(_inst)
    _check_representable(_inst)
    _check_sortable(_inst)
    _verify_compat(_inst)

    ## ALT:
    # from typing import assert_type
    # assert_type(cast(Golden[int], None), GoldenProtocol)

    # class GoldenDummy(Golden[None]):
    #     def __init__(self) -> None:
    #         super().__init__(None, self)
    #     @override
    #     def explore(self) -> Entities:
    #         raise NotImplementedError
    # inst: GoldenProtocol = GoldenDummy()


if __name__ == "__main__":  # or DEBUG:
    import inspect

    def _verify_protocol_impl(impl: type, protocol: type) -> None:
        for name, member in inspect.getmembers(protocol):
            if name.startswith("_"):
                continue
            impl_member = getattr(impl, name, None)
            assert impl_member is not None, f"{impl} missing {name}"
            # check signatures
            proto_sig = inspect.signature(member)
            impl_sig = inspect.signature(impl_member)
            assert proto_sig == impl_sig, f"{impl}.{name} signature {impl_sig} != protocol {proto_sig}"

    _verify_protocol_impl(Golden, GoldenProtocol)  # runs once at import time in dev
