from miurcache import CacheSlot
from miurcore import Entity


def test_cache_slot_keeps_entity_and_slot() -> None:
    slot = CacheSlot(entity=Entity(system="demo", handle="node"), slot=7)
    assert slot.entity.system == "demo"
    assert slot.entity.handle == "node"
    assert slot.slot == 7
