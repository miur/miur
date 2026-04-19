"""System-facing interfaces for miur."""

from typing import Protocol

from miurcore import Entity


class System(Protocol):
    """Interpret and operate on entities that belong to this system."""

    name: str

    def supports(self, entity: Entity) -> bool: ...

    def materialize(self, entity: Entity) -> object: ...

    def operate(self, entity: Entity, op: str) -> object: ...


__all__ = ["System"]
