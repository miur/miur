from typing import TYPE_CHECKING, Any, Protocol, Self

if TYPE_CHECKING:
    from .golden import Entities, Entity


# RENAME? `Named (to keep Representable for __repr__)
class Representable(Protocol):
    @property
    def name(self) -> str: ...


class Sortable(Protocol):
    def __lt__(self, other: Entity) -> bool: ...


class Addressable(Protocol):
    @property
    def loci(self) -> str: ...

    # BAD: we can either sort by loci or by name
    # def __lt__(self, other: Self) -> bool:
    #     return self.loci < other.loci


# BET?ALT: direct linked list of `EntityView.originator
#   BUT each `Entity would still need to be somehow resolved to its `EntityView
# RENAME? `Pure `ParentAware
class Locatable(Protocol):
    # RENAME? .pv=[parent|prev]view .parent .orig[inator] .back .up .off .prod[ucer]
    # NOTE: we use short ".pv" for more succint usage in code (orse use ".originator")
    @property
    def parent(self) -> Entity: ...


## ARCH:
#  * on ERROR -> return [`ErrorEntry], mixed with regular entries
#    >> if whole list ~can't be read~ -- it will result in empty list with error
#    COS: we may get multiple errors, for e.g. unreadable elements in the list
#  * on empty list -> return [], and interpret it based on `*Entry itself
#    e.g. to make different messages for empty folder and empty file
#  * if entry is atomic -> return "None", and again interpret it based on `*Entry
#    COS: behavior of HaltEntry is NOT inherent and depends on how we decide to interpret it
#    ALT:BET? remove the method itself and use getattr() to verify its presence
class Explorable(Protocol):
    # RENAME? .browse()
    def explore(self) -> Entities: ...


class Interpretable(Protocol):
    # RENAME? .can_interpret , .can_handle , .can_convert | .is_coercible | .suports , .accepts , .eligible
    #   BET? .makeable_from | .constructible_from | .convertible_from | .derivable_from | .creatable_from
    #   ALT: .can_be_made_from | .compatible_with | .understands/recognizes/resembles
    # ex~: "if ELFFile.derivable_from(FSFile)"
    @classmethod
    def creatable_from(cls, ent: Any) -> bool | None: ...

    # RENAME? .from_entity | .reinterpret | .convert | .derive | .make_from
    #   WHY: to also allow .from_<non_entity>() kind of conversions
    # ex~: "ent = ELFFile.create_from(FSFile)" | "ent = FSFile.convert_to(ELFFile)"
    @classmethod
    def create_from(cls, ent: Any) -> Self: ...

    # RENAME? get_available_interpretations()
    # MAYBE:BET? cvt ret-vals to Entities by external adapter-factory?
    def interp_as(self) -> Entities: ...


# REMOVE?
class Atomic(Addressable, Representable, Protocol):
    __slots__ = ()

    # RENAME? "ATOMIC" | "INTERPRETATION NOT ASSIGNED" | "NO INTERPRETATION" (for blob)
    explore: str = "NOT EXPLORABLE (YET)"


# RENAME? `Derivable `Composite (in contrast to `Atomic)
class GoldenStandartProtocol(
    Interpretable,
    Explorable,
    Locatable,
    Addressable,
    Sortable,
    Representable,
    Protocol,
):
    pass
