import gc
import sys
from collections import defaultdict
from dataclasses import dataclass, field
from typing import override

from ..base.golden import Entities, Entity, Golden
from ..core.text import TextEntry

MAX_DEPTH = sys.getrecursionlimit() - 100
LARGE_OBJECT_THRESHOLD = 1024 * 1024


@dataclass
class SizeResult:
    size: int
    is_incomplete: bool = False
    depth_limit_hit: bool = False


def get_deep_size(obj: object) -> SizeResult:
    seen_ids: set[int] = set()
    stack: list[tuple[object, int]] = [(obj, 0)]
    total_size = 0
    depth_limit_hit = False

    while stack:
        current, depth = stack.pop()
        obj_id = id(current)

        if obj_id in seen_ids:
            continue
        seen_ids.add(obj_id)

        if depth >= MAX_DEPTH:
            depth_limit_hit = True
            continue

        try:
            size = sys.getsizeof(current)
        except TypeError:
            continue

        total_size += size

        if isinstance(current, dict):
            for k, v in current.items():
                stack.append((k, depth + 1))
                stack.append((v, depth + 1))
        elif isinstance(current, (list, tuple)):
            for item in current:
                stack.append((item, depth + 1))
        elif hasattr(current, "__dict__"):
            stack.append((current.__dict__, depth + 1))

    return SizeResult(total_size, depth_limit_hit=depth_limit_hit)


def detect_cycles(obj: object) -> bool:
    refs = gc.get_referrers(obj)
    for ref in refs:
        if ref is obj:
            return True
    return False


def get_type_name(obj: object) -> str:
    return type(obj).__name__


def get_preview(obj: object, max_len: int = 40) -> str:
    if isinstance(obj, dict):
        return f"{{{len(obj)} items}}"
    if isinstance(obj, (list, tuple)):
        return f"[{len(obj)} items]"
    if isinstance(obj, str):
        escaped = obj.replace("\n", "\\n").replace("\t", "\\t")
        return f'"{escaped[:max_len]}"'
    r = repr(obj)
    return r[:max_len]


@dataclass
class ObjRef:
    obj: object
    size: int
    size_incomplete: bool = False
    is_cyclic: bool = False
    type_name: str = ""
    referrers: list[str] = field(default_factory=list)


@dataclass
class InstanceData:
    name: str
    obj_ref: ObjRef


class PyMemInstance(Golden[InstanceData]):
    __slots__ = ()

    def __init__(self, name: str, parent: Entity, obj: object) -> None:
        size_result = get_deep_size(obj)
        type_name = get_type_name(obj)
        preview = get_preview(obj)

        full_name = f"{type_name}: {preview}"
        if size_result.is_incomplete:
            full_name += " [INCOMPLETE]"
        if detect_cycles(obj):
            full_name += " [CYCLIC]"

        super().__init__(
            InstanceData(
                full_name,
                ObjRef(
                    obj=obj,
                    size=size_result.size,
                    size_incomplete=size_result.is_incomplete,
                    is_cyclic=detect_cycles(obj),
                    type_name=type_name,
                ),
            ),
            parent,
        )

    @override
    @property
    def name(self) -> str:
        return self._x.name

    @override
    def explore(self) -> Entities:
        ref = self._x.obj_ref
        obj = ref.obj

        yield from PyMemGcRoots._iter_referrers(f"<- {get_type_name(obj)}", self, obj)

        try:
            referents = gc.get_referents(obj)
            for referent in referents:
                yield PyMemRef(f"-> {get_type_name(referent)}", self, referent)
        except (TypeError, ReferenceError):
            pass


class PyMemRef(Golden[tuple[str, object]]):
    __slots__ = ()

    def __init__(self, name: str, parent: Entity, obj: object) -> None:
        super().__init__((name, obj), parent)

    @override
    def explore(self) -> Entities:
        name, obj = self._x
        yield PyMemInstance(f"obj @0x{id(obj):x}", self, obj)


@dataclass
class TypeGroupData:
    name: str
    typ: type
    objs: list[object]


class PyMemTypeGroup(Golden[TypeGroupData]):
    __slots__ = ()

    def __init__(
        self, name: str, parent: Entity, typ: type, objs: list[object]
    ) -> None:
        super().__init__(TypeGroupData(name, typ, objs), parent)

    @override
    def explore(self) -> Entities:
        for obj in self._x.objs:
            yield PyMemInstance(f"@0x{id(obj):x}", self, obj)


class PyMemByType(Golden[str]):
    __slots__ = ()

    def __init__(self, parent: Entity) -> None:
        super().__init__("[by_type]", parent)

    @override
    def explore(self) -> Entities:
        by_type: dict[type, list[object]] = defaultdict(list)

        for obj in gc.get_objects():
            t = type(obj)
            by_type[t].append(obj)

        sorted_types = sorted(
            by_type.items(),
            key=lambda kv: sum(get_deep_size(o).size for o in kv[1]),
            reverse=True,
        )

        for typ, objs in sorted_types:
            total_size = sum(get_deep_size(o).size for o in objs)
            name = f"{typ.__name__} ({len(objs)} objs, {total_size // 1024}KB)"
            yield PyMemTypeGroup(name, self, typ, objs)


@dataclass
class ModuleGroupData:
    name: str
    mod_name: str
    objs: list[object]


class PyMemModuleGroup(Golden[ModuleGroupData]):
    __slots__ = ()

    def __init__(
        self, name: str, parent: Entity, mod_name: str, objs: list[object]
    ) -> None:
        super().__init__(ModuleGroupData(name, mod_name, objs), parent)

    @override
    def explore(self) -> Entities:
        for obj in self._x.objs:
            yield PyMemInstance(f"@0x{id(obj):x}", self, obj)


class PyMemByModule(Golden[str]):
    __slots__ = ()

    def __init__(self, parent: Entity) -> None:
        super().__init__("[by_module]", parent)

    @override
    def explore(self) -> Entities:
        by_module: dict[str, list[object]] = defaultdict(list)

        for obj in gc.get_objects():
            mod = getattr(type(obj), "__module__", "__unknown__")
            by_module[mod].append(obj)

        def total_size(objs: list[object]) -> int:
            return sum(get_deep_size(o).size for o in objs)

        sorted_modules = sorted(
            by_module.items(),
            key=lambda kv: total_size(kv[1]),
            reverse=True,
        )

        for mod_name, objs in sorted_modules:
            size_kb = total_size(objs) // 1024
            name = f"{mod_name} ({len(objs)} objs, {size_kb}KB)"
            yield PyMemModuleGroup(name, self, mod_name, objs)


class PyMemLarge(Golden[str]):
    __slots__ = ()

    def __init__(self, parent: Entity) -> None:
        super().__init__("[large_objects]", parent)

    @override
    def explore(self) -> Entities:
        large_objs = []
        for obj in gc.get_objects():
            size_result = get_deep_size(obj)
            if size_result.size >= LARGE_OBJECT_THRESHOLD:
                large_objs.append((obj, size_result.size))

        large_objs.sort(key=lambda x: x[1], reverse=True)

        for obj, size in large_objs:
            yield PyMemInstance(f"@0x{id(obj):x}", self, obj)


class PyMemGcRoots(Golden[str]):
    __slots__ = ()

    def __init__(self, parent: Entity) -> None:
        super().__init__("[gc_roots]", parent)

    @override
    def explore(self) -> Entities:
        yield from self._explore_garbage()

        yield from self._explore_frames()

        yield from self._explore_modules()

        yield from self._explore_builtins()

    def _explore_garbage(self) -> Entities:
        for i, obj in enumerate(gc.garbage):
            yield PyMemInstance(f"gc.garbage[{i}]", self, obj)

    def _explore_frames(self) -> Entities:
        for frame_id, frame in sys._current_frames().items():
            yield from self._explore_frame_recursive(frame, f"frame@{frame_id}")

    def _explore_frame_recursive(self, frame, name: str) -> Entities:
        yield PyMemRef(f"{name}.f_locals", self, frame.f_locals)
        yield PyMemRef(f"{name}.f_globals", self, frame.f_globals)

        if frame.f_back:
            yield from self._explore_frame_recursive(frame.f_back, f"{name}.f_back")

    def _explore_modules(self) -> Entities:
        for name, mod in sys.modules.items():
            if mod is None:
                continue
            try:
                yield PyMemRef(f"module:{name}", self, vars(mod))
            except (TypeError, AttributeError):
                pass

    def _explore_builtins(self) -> Entities:
        import builtins

        yield PyMemRef("builtins", self, vars(builtins))

    @staticmethod
    def _iter_referrers(name: str, parent: Entity, obj: object) -> Entities:
        try:
            referrers = gc.get_referrers(obj)
            for referrer in referrers:
                if referrer is parent._x:
                    continue
                if isinstance(referrer, dict):
                    for k, v in referrer.items():
                        if v is obj:
                            yield PyMemRef(f"{name}['{k}']", parent, referrer)
                elif isinstance(referrer, list):
                    for i, v in enumerate(referrer):
                        if v is obj:
                            yield PyMemRef(f"{name}[{i}]", parent, referrer)
                else:
                    yield PyMemRef(
                        f"{name} <- {get_type_name(referrer)}", parent, referrer
                    )
        except (TypeError, ReferenceError):
            pass


class PyMemStat(Golden[str]):
    __slots__ = ()

    def __init__(self, parent: Entity) -> None:
        super().__init__("[stats]", parent)

    @override
    def explore(self) -> Entities:
        all_objs = gc.get_objects()
        total_count = len(all_objs)

        total_size = 0
        type_counts: dict[str, int] = defaultdict(int)
        type_sizes: dict[str, int] = defaultdict(int)

        for obj in all_objs:
            size_result = get_deep_size(obj)
            total_size += size_result.size
            tn = get_type_name(obj)
            type_counts[tn] += 1
            type_sizes[tn] += size_result.size

        yield TextEntry(f"Total objects: {total_count}", self)
        yield TextEntry(f"Total size: {total_size // (1024*1024)} MB", self)
        yield TextEntry(f"gc.garbage: {len(gc.garbage)}", self)
        yield TextEntry(f"Modules: {len(sys.modules)}", self)
        yield TextEntry("", self)

        yield TextEntry("Top by count:", self)
        for tn, count in sorted(type_counts.items(), key=lambda x: -x[1])[:10]:
            yield TextEntry(f"  {tn}: {count}", self)

        yield TextEntry("", self)
        yield TextEntry("Top by size:", self)
        for tn, size in sorted(type_sizes.items(), key=lambda x: -x[1])[:10]:
            yield TextEntry(f"  {tn}: {size // 1024} KB", self)


class PyMemRoot(Golden[str]):
    __slots__ = ()

    def __init__(self, parent: Entity) -> None:
        super().__init__("[memory]", parent)

    @override
    def explore(self) -> Entities:
        return [
            PyMemGcRoots(self),
            ## FAIL:PERF: very slow, takes like ~20s
            # PyMemByType(self),
            # PyMemByModule(self),
            # PyMemLarge(self),
            # PyMemStat(self),
        ]
