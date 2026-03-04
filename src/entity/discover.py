from typing import Any, TypeAliasType, cast, overload


_ALREADY_SCANNED: set[str] = set()
_IGNORE_PREFIXES: tuple[str, ...] = ("_", "test", "intern", "mock")


# ALT:(miur-plugins): use "importlib.metadata" to access [project.entry-points."miur.plugins"]
#   /w/g18-*/miur-plugins.nou
def ensure_all_modules_loaded_and_registered(pkg_base: str = "") -> None:
    """
    Standard high-performance module loader optimized for monorepos.
    """
    if pkg_base in _ALREADY_SCANNED:
        return

    if not pkg_base:
        if not __package__:
            raise ValueError("pkg_base must be provided if called from a non-package.")
        ## BAD: it's "src" inof "miur"
        # log.info(f"{__package__=}")
        pkg_base = __package__.rpartition(".")[0]

    import pkgutil
    import sys
    from importlib import import_module

    try:
        # OR: entpkg = import_module(".entity", __package__)
        root_package = import_module(pkg_base)
    except ImportError:
        return

    ## FAIL: you need to export all modnms into __all__ first
    # import inspect
    # for nm, val in inspect.getmembers_static(entpkg, inspect.ismodule):
    #     log.info((nm, val))

    ## WARN:RQ: import/declare all classess -- to register them
    # for fnm in os.listdir(fs.dirname(entpkg.__file__)):
    #     if fnm == "__init__.py" or fnm[-3:] != ".py":
    #         continue
    #     # log.info(("." + fnm[:-3], entpkg.__package__))
    #     _ = importlib.import_module("." + fnm[:-3], entpkg.__package__)

    ## ALT:IMPL -- OR:USE: "pkgutil.iter_modules" to avoid unnecessary recursion
    # import importlib.resources
    # traversable_root = importlib.resources.files(pkg_base)
    # with importlib.resources.as_file(traversable_root) as pkg_fs_path:
    #     for path in pkg_fs_path.rglob("*.py"):
    #         # Skip dunder files like __init__.py or __main__.py
    #         if path.name.startswith("__"):
    #             continue
    #         rel_path = path.relative_to(pkg_fs_path).with_suffix("")
    #         module_name = f"{pkg_base}.{'.'.join(rel_path.parts)}"
    #         if module_name not in sys.modules:
    #             try:
    #                 importlib.import_module(module_name)
    #             except ImportError:
    #                 continue

    # INFO: pkgutil uses the same machinery Python's own import system uses,
    #   making it more robust against edge cases like namespace packages.
    # ATT: walk_packages() already imports the modules(top-level) to inspect them
    for info in pkgutil.walk_packages(root_package.__path__, f"{pkg_base}."):
        if info.name in sys.modules:
            continue
        if any(info.name.startswith(p) for p in _IGNORE_PREFIXES):
            continue
        try:
            import_module(info.name)
        except (ImportError, AttributeError, RuntimeError):
            # FUT: log this, otherwise skip
            continue

    _ALREADY_SCANNED.add(pkg_base)


@overload
def get_all_subclasses[T](cls: type[T], again: bool = False) -> list[type[T]]: ...


@overload
# NOTE: !mypy>=1.11 can infer T from the TypeAliasType dynamically
def get_all_subclasses[T](cls: TypeAliasType, again: bool = False) -> list[type[T]]: ...


def get_all_subclasses[T](
    cls: type[T] | TypeAliasType,
    again: bool = False,
) -> list[type[T]]:
    """
    Iteratively finds all subclasses using a stack.
    """
    state = get_all_subclasses.__dict__
    if again or not state.get("init"):
        ensure_all_modules_loaded_and_registered()
        state["init"] = True
        state["cache"] = {}
    # NOTE:(type[Any]): cache contains not only T, but multiple unrelated hierarchies
    cache: dict[type[Any], list[type[Any]]] = state["cache"]

    # FIXED:(__value__):ERR: arg1 has incompatible type "TypeAliasType"; expected "type"
    #   ALT: inof "type Entity = ..." use direct assignment "Entity = Golden[Any]"
    # INFO: In modern Python, one TypeAliasType can point to another TypeAliasType
    root_cls = cls
    while isinstance(root_cls, TypeAliasType):
        root_cls = root_cls.__value__
    # root_cls = cast(type[T], root_cls)

    ## ALSO? guard against AttributeError for Union/Literal types that don't support __subclasses__
    # if not isinstance(root_cls, type):
    #     return []

    if root_cls in cache:
        return cast(list[type[T]], cache[root_cls])

    # ALG: BFS traversal
    #   INFO: returning "seen" set is slightly faster, but we lose predictable ordering in logs
    #   MAYBE: stop inhereting from `AutoRegistered as we have __subclasses__ ?
    # WARN:FUT:(PEP-690): might be empty until the module is actually accessed, not just imported.
    xs = root_cls.__subclasses__()
    seen = set(xs)
    add = seen.add
    append = xs.append
    i = 0
    while i < len(xs):
        for subcls in xs[i].__subclasses__():
            if subcls not in seen:  # <FIXED: diamond inheritance
                add(subcls)
                append(subcls)
        i += 1

    cache[root_cls] = xs
    return xs
