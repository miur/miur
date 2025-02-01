from typing import TYPE_CHECKING, Iterable, Protocol, Self

if TYPE_CHECKING:
    from ...ui.view import EntityView
    from .golden import Golden


# RENAME? `Named (to keep Representable for __repr__)
class Representable(Protocol):
    @property
    def name(self) -> str: ...

    # MOVE? "Sortable" ?
    def __lt__(self, other: "Self") -> bool:
        return self.name < other.name


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
    def pv(self) -> "EntityView": ...


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
    def explore(self) -> "Iterable[Golden]": ...


# REMOVE?
class Atomic(Addressable, Representable, Protocol):
    __slots__ = ()

    # RENAME? "ATOMIC" | "INTERPRETATION NOT ASSIGNED" | "NO INTERPRETATION" (for blob)
    explore: str = "NOT EXPLORABLE (YET)"


# RENAME? `Derivable `Composite
class Standart(Explorable, Locatable, Addressable, Representable, Protocol):
    pass
