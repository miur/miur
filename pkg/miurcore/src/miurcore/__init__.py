"""Core domain package for miur."""

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Entity:
    """Entity identity as system name plus opaque system handle."""

    system: str
    handle: str


__all__ = ["Entity"]
