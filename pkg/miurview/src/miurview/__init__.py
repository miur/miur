"""View-level abstractions for miur."""

from dataclasses import dataclass

from miurcore import Entity


@dataclass(frozen=True, slots=True)
class Cursor:
    """Current focused entity within a projected view."""

    entity: Entity


__all__ = ["Cursor"]
