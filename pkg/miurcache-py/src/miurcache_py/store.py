"""Packed storage primitives for resident graph cache state."""

from __future__ import annotations

from array import array
from collections.abc import Iterable

from miurcore import ChildSlice
from miurcore import EntityRecord

_U32_ARRAY_CODE = "I"
_U64_ARRAY_CODE = "Q"


class PackedEntityColumns:
    """Structure-of-arrays storage for six u32 entity fields."""

    def __init__(self) -> None:
        self._columns = tuple(array(_U32_ARRAY_CODE) for _ in range(6))

    def __len__(self) -> int:
        return len(self._columns[0])

    def append(self, record: EntityRecord) -> int:
        slot = len(self)
        for column, value in zip(self._columns, record.as_tuple(), strict=True):
            column.append(value)
        return slot

    def write(self, slot: int, record: EntityRecord) -> None:
        self._require_slot(slot)
        for column, value in zip(self._columns, record.as_tuple(), strict=True):
            column[slot] = value

    def read(self, slot: int) -> EntityRecord:
        self._require_slot(slot)
        return EntityRecord(*(column[slot] for column in self._columns))

    def _require_slot(self, slot: int) -> None:
        if not 0 <= slot < len(self):
            raise IndexError(f"slot out of range: {slot}")


class PackedAdjacency:
    """Append-only adjacency storage with per-slot child spans."""

    def __init__(self) -> None:
        self._starts = array(_U64_ARRAY_CODE)
        self._stops = array(_U64_ARRAY_CODE)
        self._children = array(_U32_ARRAY_CODE)

    def write(self, slot: int, child_ids: Iterable[int]) -> ChildSlice:
        self._ensure_slot(slot)
        start = len(self._children)
        for child_id in child_ids:
            if child_id < 0:
                raise ValueError(f"child ids must be non-negative, got {child_id!r}")
            self._children.append(child_id)
        stop = len(self._children)
        self._starts[slot] = start
        self._stops[slot] = stop
        return ChildSlice(start=start, stop=stop)

    def read(self, slot: int) -> tuple[int, ...]:
        self._require_slot(slot)
        start = self._starts[slot]
        stop = self._stops[slot]
        return tuple(self._children[start:stop])

    def span(self, slot: int) -> ChildSlice:
        self._require_slot(slot)
        return ChildSlice(start=self._starts[slot], stop=self._stops[slot])

    def _ensure_slot(self, slot: int) -> None:
        while len(self._starts) <= slot:
            self._starts.append(0)
            self._stops.append(0)

    def _require_slot(self, slot: int) -> None:
        if not 0 <= slot < len(self._starts):
            raise IndexError(f"slot out of range: {slot}")
