import pytest

from miurcore import ChildSlice
from miurcore import Entity
from miurcore import EntityRecord


def test_entity_exposes_system_and_handle() -> None:
    entity = Entity(system="localfs", handle="/tmp/x")
    assert entity.system == "localfs"
    assert entity.handle == "/tmp/x"


def test_entity_record_keeps_six_u32_values() -> None:
    record = EntityRecord(1, 2, 3, 4, 5, 6)
    assert record.as_tuple() == (1, 2, 3, 4, 5, 6)


def test_entity_record_rejects_values_outside_u32() -> None:
    with pytest.raises(ValueError):
        EntityRecord(0, 0, 0, 0, 0, 1 << 32)


def test_child_slice_reports_count() -> None:
    span = ChildSlice(start=4, stop=9)
    assert span.count == 5
