import os
from abc import ABCMeta, abstractmethod
from itertools import islice
from typing import Iterable, Self

# pylint:disable=too-few-public-methods


# NOTE: entity may have no "name" if it's simply unnamed VM memory areas
#   WARN: entity may not have even "luid" (like addr local to specific VM)
#     e.g. network pkts in socket can be identical and yet different (like "was_invalidated" ntf)
#   WARN: entity may not have enen characteristic of "order" e.g. for randomly delivered UDP pkts
class Entity(metaclass=ABCMeta):
    # NOTE: "preferred" only has sense in the ctx of viewer/widget/workflow
    #   i.e. we may have different "preferred" actions based on current widget
    #     IDEA: use dict() when you have more than one preferred value
    #   USAGE: widget overlay should fallback to "most logical defaults", which is "preferred"
    #     RENAME? → "default="
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


# DECI: name after "outer container" (=DirectoryListing) ?
#   OR after "common ancestor of elements" (=InodeListing)
#   BUT: how to re-iterate over *generated* listing -- should we cache items inside XXXListing ?
#     BUT: for very long folders/IRC -- we shouldn't cache whole lists at once (more RAM, more latency)
#         ~~ MAYBE: store only str(paths) w/o converting them to Entity/CachedProxy to reduce RAM footprint
#       BUT: lists usually need to be sorted -- it's hard to not get whole list first to sort once
#         ~~ though, IRC is ts-appended, and file listing can be insert-sorted...
# RENAME? FSEntryEntity, PathEntry
class InodeListing(object):
    def __init__(self, path: Path) -> None:
        self._path = path

    def get_stats(self) -> Iterable[StatEntity]:
        yield from StatEntity(os.stat(self._path))


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


# WARN: Entity should support multitude of own obj actions
#   *and* its parent actions e.g. "get_stats", "get_lines"
# BAD: backed to file Elf may have dual representation in RAM (for running process)
class ElfEntity(FileEntity):
    preferred = "lines"

    # def get_sections|segments|symbols(self):
    #     pass


# BAD: can be Memory-based (STDIN), Stream-based (HTTP), or DOM-based (lang native structs)
#   ALSO: can at any moment become File-backed or DB-backed (for persistence) and sync changes into storage
#   THINK: how to use dynamic composition OR mix-ins to approximate everything ?
#     IDEA: assign list of "traits" to each Entity -- like supported API sets / generic procedures
class JsonEntity(FileEntity):
    preferred = "highlights/sketch/majorants"

    # def get_json_dict|list|keys|values(self):
    #     pass


# ALSO: streams/cmpts/alt-views for same identity
#   .jpg -- FileContent, FileInode, FileMetadata, FilePreview, FileHash


## !WF! Everything boils down to how API will be used by Widget
##   -- i.e. how to efficiently access those classes above
