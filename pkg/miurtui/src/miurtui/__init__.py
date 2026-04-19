"""Terminal UI entry points for miur."""

from miurview import Cursor


def render(cursor: Cursor) -> str:
    """Render a minimal cursor status line."""
    return f"cursor={cursor.entity.system}:{cursor.entity.handle}"


__all__ = ["render"]
