"""Core domain package for miur."""

from dataclasses import dataclass

_U32_MAX = (1 << 32) - 1


def _require_u32(value: int, field: str) -> None:
    if not 0 <= value <= _U32_MAX:
        raise ValueError(f"{field} must fit into u32, got {value!r}")


@dataclass(frozen=True, slots=True)
class Entity:
    """Entity identity as system name plus opaque system handle."""

    system: str
    handle: str


@dataclass(frozen=True, slots=True)
class EntityRecord:
    """Compact fixed-width entity payload represented as six u32 fields."""

    c0: int
    c1: int
    c2: int
    c3: int
    c4: int
    c5: int

    def __post_init__(self) -> None:
        _require_u32(self.c0, "c0")
        _require_u32(self.c1, "c1")
        _require_u32(self.c2, "c2")
        _require_u32(self.c3, "c3")
        _require_u32(self.c4, "c4")
        _require_u32(self.c5, "c5")

    def as_tuple(self) -> tuple[int, int, int, int, int, int]:
        return (self.c0, self.c1, self.c2, self.c3, self.c4, self.c5)


@dataclass(frozen=True, slots=True)
class ChildSlice:
    """Half-open child span inside an adjacency buffer."""

    start: int
    stop: int

    def __post_init__(self) -> None:
        if self.start < 0:
            raise ValueError(f"start must be non-negative, got {self.start!r}")
        if self.stop < self.start:
            raise ValueError(
                f"stop must be greater than or equal to start, got {self.stop!r}"
            )

    @property
    def count(self) -> int:
        return self.stop - self.start


__all__ = ["ChildSlice", "Entity", "EntityRecord"]
