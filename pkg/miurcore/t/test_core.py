from miurcore import Entity


def test_entity_exposes_system_and_handle() -> None:
    entity = Entity(system="localfs", handle="/tmp/x")
    assert entity.system == "localfs"
    assert entity.handle == "/tmp/x"
