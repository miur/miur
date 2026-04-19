from miurcore import Entity
from miurview import Cursor


def test_cursor_holds_entity() -> None:
    cursor = Cursor(Entity(system="demo", handle="focus"))
    assert cursor.entity.system == "demo"
    assert cursor.entity.handle == "focus"
