"""Local filesystem system for miur."""

from pathlib import Path

from miurcore import Entity


def entity_from_path(path: str | Path) -> Entity:
    """Build a local filesystem entity with an opaque handle."""
    return Entity(system="localfs", handle=str(Path(path)))
