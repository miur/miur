from dataclasses import dataclass

from miurcache import CacheSlot
from miurcache import PackedAdjacency
from miurcache import PackedEntityColumns
from miurcache import ResidentEntity
from miurcache import ResidentGraphCache
from miurcore import Entity
from miurcore import EntityRecord


def test_cache_slot_keeps_entity_and_slot() -> None:
    slot = CacheSlot(entity=Entity(system="demo", handle="node"), slot=7)
    assert slot.entity.system == "demo"
    assert slot.entity.handle == "node"
    assert slot.slot == 7


def test_packed_entity_columns_round_trip_record() -> None:
    store = PackedEntityColumns()
    slot = store.append(EntityRecord(1, 2, 3, 4, 5, 6))
    assert slot == 0
    assert store.read(slot).as_tuple() == (1, 2, 3, 4, 5, 6)


def test_packed_adjacency_round_trip_children() -> None:
    adjacency = PackedAdjacency()
    span = adjacency.write(0, [10, 11, 12])
    assert span.start == 0
    assert span.stop == 3
    assert adjacency.read(0) == (10, 11, 12)


@dataclass
class _FakeSpillStore:
    saved: list[ResidentEntity]

    def persist(self, resident: ResidentEntity) -> None:
        self.saved.append(resident)


def test_resident_graph_cache_reads_back_record_and_children() -> None:
    cache = ResidentGraphCache(capacity=2)
    entity = Entity(system="demo", handle="node-1")
    slot = cache.store(entity, EntityRecord(7, 8, 9, 10, 11, 12), [20, 21])
    resident = cache.read(entity)
    assert slot == 0
    assert resident is not None
    assert resident.slot == 0
    assert resident.record.as_tuple() == (7, 8, 9, 10, 11, 12)
    assert resident.children == (20, 21)


def test_resident_graph_cache_evicts_lru_and_spills_unique_entities() -> None:
    spill_store = _FakeSpillStore(saved=[])
    cache = ResidentGraphCache(capacity=1, spill_store=spill_store)
    first = Entity(system="demo", handle="first")
    second = Entity(system="demo", handle="second")

    cache.store(first, EntityRecord(1, 1, 1, 1, 1, 1), [2, 3], unique=True)
    cache.store(second, EntityRecord(9, 9, 9, 9, 9, 9), [4])

    assert cache.read(first) is None
    assert cache.read(second) is not None
    assert len(spill_store.saved) == 1
    assert spill_store.saved[0].entity == first
    assert spill_store.saved[0].children == (2, 3)
