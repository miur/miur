"""Application bootstrap and runtime layer for miur."""

from dataclasses import dataclass

from miurcore import Entity
from miurtui import render
from miurview import Cursor


@dataclass(frozen=True, slots=True)
class AppSession:
    """Minimal application session state for runtime wiring."""

    focus: Entity


def run(session: AppSession) -> str:
    """Run a minimal app session by wiring focus into the TUI renderer."""
    return render(Cursor(session.focus))


__all__ = ["AppSession", "run"]
