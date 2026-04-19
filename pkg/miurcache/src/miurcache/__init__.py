"""Cache-layer primitives for miur."""

from dataclasses import dataclass

from miurcore import Entity


@dataclass(frozen=True, slots=True)
class CacheSlot:
    """Resident slot for an entity in the live working set."""

    entity: Entity
    slot: int


__all__ = ["CacheSlot"]
