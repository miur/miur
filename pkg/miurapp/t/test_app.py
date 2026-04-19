from miurapp import AppSession
from miurapp import run
from miurcore import Entity


def test_run_renders_focused_entity() -> None:
    session = AppSession(focus=Entity(system="demo", handle="root"))
    assert run(session) == "cursor=demo:root"
