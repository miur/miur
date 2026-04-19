from miurcore import Entity
from miurtui import render
from miurview import Cursor


def test_render_formats_cursor_identity() -> None:
    cursor = Cursor(Entity(system="demo", handle="focus"))
    assert render(cursor) == "cursor=demo:focus"
