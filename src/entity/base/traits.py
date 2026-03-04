# RENAME? proto.py
from typing import TYPE_CHECKING, Any, Protocol, Self, override


if TYPE_CHECKING:
    from .golden import Entities, Entity

# type Entity = GoldenProtocol  # OR: GoldenProto
# type Entities = Iterable[GoldenProtocol]


# RENAME? `Named (to keep Representable for __repr__)
class Representable(Protocol):
    @property
    def name(self) -> str: ...


class Sortable(Protocol):
    def __lt__(self, other: Entity) -> bool: ...


# BET?ALT: direct linked list of `EntityView.originator
#   BUT each `Entity would still need to be somehow resolved to its `EntityView
# RENAME? `Pure `ParentAware `Locatable
class Backtrackable(Protocol):
    # RENAME? .pv=[parent|prev]view .parent .orig[inator] .back .up .off .prod[ucer]
    # NOTE: we use short ".pv" for more succint usage in code (orse use ".orig[inator]")
    @property
    def parent(self) -> Entity: ...


class Locatable(Protocol):
    @property
    def loci(self) -> str: ...


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


# REMOVE?: *all objects are black-boxes* -- until they can be .interp_as() to some structure
class Atomic(Locatable, Representable, Protocol):
    __slots__ = ()

    # RENAME? "ATOMIC" | "INTERPRETATION NOT ASSIGNED" | "NO INTERPRETATION" (for blob)
    explore: str = "NOT EXPLORABLE (YET)"


# RENAME? `Derivable `Composite (in contrast to `Atomic)
class GoldenProtocol(
    # Interpretable,
    Explorable,
    Locatable,
    Backtrackable,
    Sortable,
    Representable,
    Protocol,
):
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
