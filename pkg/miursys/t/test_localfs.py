from pathlib import Path

from miursys.localfs import entity_from_path


def test_entity_from_path_builds_localfs_entity() -> None:
    entity = entity_from_path(Path("/tmp/demo"))
    assert entity.system == "localfs"
    assert entity.handle == "/tmp/demo"
