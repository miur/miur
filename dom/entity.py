import os
from abc import ABCMeta, abstractmethod
from itertools import islice
from typing import Iterable, Self

# pylint:disable=too-few-public-methods


class Entity(metaclass=ABCMeta):
    preferred = "actions"
    # @property
    # @abstractmethod
    # def name(self) -> str:
    #     raise NotImplementedError
    #
    # @property
    # @abstractmethod
    # def size(self) -> int:
    #     raise NotImplementedError
    #
    # @abstractmethod
    # def __str__(self) -> str:
    #     raise NotImplementedError

    def __iter__(self) -> Iterable[Self]:
        # ALT: inspect.getmembers(self, lambda v: not v.__name__.startswith('__'))
        return (Self(v) for nm, v in vars(self).items() if not nm.startswith("__"))

    # def get_list_of_entities(self) -> Iterable[Self]:
    #     pass
    #
    # def get_named_entity(self, nm) -> Self:
    #     pass
    # def execute(self) -> Entity:
    #     pass


# TODO: build complex *concrete* hierarchy, and then generalize to common repr


class EntityEntity(Entity):
    pass


class StatEntity(Entity):
    pass


class InodeEntity(Entity):
    def __init__(self, path: Path) -> None:
        self._path = path

    def get_stats(self) -> Iterable[StatEntity]:
        yield from StatEntity(os.stat(self._path))


class FileEntity(InodeEntity):
    preferred = "lines"

    def get_lines(self, beg: int, end: int) -> Iterable[TextLineEntity | HexBlobEntity]:
        with open(self._path, "r", encoding="utf-8") as f:
            try:
                yield from (TextLineEntity(l.rstrip()) for l in islice(f, beg, end))
            except UnicodeDecodeError:
                for i in range(beg, end):
                    f.seek(i * 16, 0)
                    yield HexBlobEntity(f.read(16))  # .hex()

    # WARN: Entity should support multitude of own obj actions
    #   *and* its parent actions e.g. "get_stats", "get_lines"
    ## .elf
    # def get_sections|segments|symbols(self):
    #     pass
    ## .json
    # def get_json_dict|list|keys|values(self):
    #     pass


class DirectoryEntity(InodeEntity):
    preferred = "inodes"

    def get_inodes(self) -> Iterable[Self | FileEntity]:
        for x in os.scandir(self._path):
            if x.is_dir():
                yield DirectoryEntity(x.path)
            else:
                yield FileEntity(x.path)

    def get_actions(self) -> Iterable[EntityEntity]:
        yield ShowActionEntity("FSListWidget", "FSView", "get_listing")
        yield ShowActionEntity("KV(keyval)ListWidget", "AttrView", "get_stats")
        yield ShowActionEntity("VirtualListWidget", "EntityView", "get_actions")
