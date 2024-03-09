import os
from abc import ABCMeta
from itertools import islice
from pathlib import Path
from typing import Iterable, Self

# pylint:disable=too-few-public-methods


class Entity(metaclass=ABCMeta):
    default = "actionlist"


class ActionEntity(Entity):
    default = "execute"


class InodeEntity(Entity):
    def __init__(self, path: Path) -> None:
        self._path = path


class DirEntity(InodeEntity):
    default = "inodes"

    def get_inodes(self) -> Iterable[Self | FileEntity]:
        for x in os.scandir(self._path):
            if x.is_dir():
                yield DirEntity(x.path)
            else:
                yield FileEntity(x.path)

    def get_actions(self) -> Iterable[ActionEntity]:
        yield ShowActionEntity("FSListWidget", "FSView", "get_listing")
        yield ShowActionEntity("KV(keyval)ListWidget", "AttrView", "get_stats")
        yield ShowActionEntity("VirtualListWidget", "EntityView", "get_actions")


class FileEntity(InodeEntity):
    default = "lines"

    def get_lines(self, beg: int, end: int) -> Iterable[TextLineEntity | HexBlobEntity]:
        with open(self._path, "r", encoding="utf-8") as f:
            try:
                yield from (TextLineEntity(l.rstrip()) for l in islice(f, beg, end))
            except UnicodeDecodeError:
                for i in range(beg, end):
                    f.seek(i * 16, 0)
                    yield HexBlobEntity(f.read(16))  # .hex()

    def get_stats(self) -> Iterable[StatEntity]:
        yield from StatEntity(os.stat(self._path))
