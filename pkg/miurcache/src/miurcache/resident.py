"""Resident cache and eviction logic for partially materialized graph state."""

from __future__ import annotations

from collections import OrderedDict
from collections.abc import Iterable
from dataclasses import dataclass
from typing import Protocol

from miurcore import Entity, EntityRecord

from .store import PackedAdjacency, PackedEntityColumns


@dataclass(frozen=True, slots=True)
class ResidentEntity:
    """Fully materialized entity payload stored in the resident cache."""

    entity: Entity
    slot: int
    record: EntityRecord
    children: tuple[int, ...]
    unique: bool


class SpillStore(Protocol):
    """Persistence boundary for evicted unique entities."""

    def persist(self, resident: ResidentEntity) -> None:
        """Persist an entity that must survive eviction."""


class ResidentGraphCache:
    """Bounded resident cache keyed by entity identity with LRU eviction."""

    def __init__(self, capacity: int, spill_store: SpillStore | None = None) -> None:
        if capacity <= 0:
            raise ValueError(f"capacity must be positive, got {capacity!r}")
        self.capacity = capacity
        self.spill_store = spill_store
        self._records = PackedEntityColumns()
        self._adjacency = PackedAdjacency()
        self._entity_to_slot: dict[Entity, int] = {}
        self._slot_to_entity: list[Entity | None] = []
        self._unique_by_slot: list[bool] = []
        self._free_slots: list[int] = []
        self._lru: OrderedDict[Entity, None] = OrderedDict()

    def __len__(self) -> int:
        return len(self._entity_to_slot)

    def store(
        self,
        entity: Entity,
        record: EntityRecord,
        children: Iterable[int] = (),
        *,
        unique: bool = False,
    ) -> int:
        slot = self._entity_to_slot.get(entity)
        if slot is None:
            slot = self._allocate_slot()
            if slot == len(self._slot_to_entity):
                self._slot_to_entity.append(entity)
                self._unique_by_slot.append(unique)
                self._records.append(record)
            else:
                self._slot_to_entity[slot] = entity
                self._unique_by_slot[slot] = unique
                self._records.write(slot, record)
            self._entity_to_slot[entity] = slot
        else:
            self._records.write(slot, record)
            self._unique_by_slot[slot] = unique
        self._adjacency.write(slot, children)
        self.touch(entity)
        return slot

    def read(self, entity: Entity) -> ResidentEntity | None:
        slot = self._entity_to_slot.get(entity)
        if slot is None:
            return None
        self.touch(entity)
        return self._resident_from_slot(slot)

    def touch(self, entity: Entity) -> None:
        if entity not in self._entity_to_slot:
            raise KeyError(f"entity is not resident: {entity!r}")
        self._lru.pop(entity, None)
        self._lru[entity] = None

    def evict(self, entity: Entity) -> ResidentEntity | None:
        slot = self._entity_to_slot.pop(entity, None)
        if slot is None:
            return None
        self._lru.pop(entity, None)
        resident = self._resident_from_slot(slot)
        self._slot_to_entity[slot] = None
        self._unique_by_slot[slot] = False
        self._free_slots.append(slot)
        if resident.unique and self.spill_store is not None:
            self.spill_store.persist(resident)
        return resident

    def slots(self) -> tuple[tuple[Entity, int], ...]:
        return tuple((entity, slot) for entity, slot in self._entity_to_slot.items())

    def _allocate_slot(self) -> int:
        if self._free_slots:
            return self._free_slots.pop()
        if len(self) < self.capacity:
            return len(self._slot_to_entity)
        entity = next(iter(self._lru))
        evicted = self.evict(entity)
        if evicted is None:
            raise RuntimeError("failed to evict resident entity")
        reclaimed = self._free_slots.pop()
        if reclaimed != evicted.slot:
            raise RuntimeError(
                f"slot reuse bookkeeping diverged: expected {evicted.slot}, got {reclaimed}"
            )
        return evicted.slot

    ec.

    def _resident_from_slot(self, slot: int) -> ResidentEntity:
        entity = self._slot_to_entity[slot]
        if entity is None:
            raise RuntimeError(f"slot {slot} is not assigned")
        return ResidentEntity(
            entity=entity,
            slot=slot,
            record=self._records.read(slot),
            children=self._adjacency.read(slot),
            unique=self._unique_by_slot[slot],
        )
