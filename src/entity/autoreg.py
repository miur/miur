def ensure_all_modules_loaded_and_registered(pkg_base: str = "") -> None:
    """Standard recursive loader using importlib.resources."""
    import importlib
    import importlib.resources
    import sys

    if not pkg_base:
        pkg_base = __package__.rpartition(".")[0]
    pkg_files = importlib.resources.files(pkg_base)
    for path in pkg_files.rglob("*.py"):
        if path.name.startswith("__"):
            continue
        rel_path = path.relative_to(pkg_files).with_suffix("")
        module_name = f"{pkg_base}.{'.'.join(rel_path.parts)}"
        if module_name not in sys.modules:
            try:
                importlib.import_module(module_name)
            except ImportError:
                continue


def get_all_subclasses(root_cls: type, again: bool = False) -> list[type]:
    """
    Iteratively finds all subclasses using a stack.
    """
    state = get_all_subclasses.__dict__
    if again or not state.setdefault("init", False):
        ensure_all_modules_loaded_and_registered()
        state["init"] = True
        state["cache"] = {}

    cache = state["cache"]
    if root_cls in cache:
        return cache[root_cls]

    # ALG: BFS traversal
    #   INFO: returning "seen" set is slightly faster, but we lose predictable ordering in logs
    #   MAYBE: stop inhereting from `AutoRegistered as we have __subclasses__ ?
    xs = root_cls.__subclasses__()
    seen = set(xs)
    add = seen.add
    append = xs.append
    i = 0
    while i < len(xs):
        for cls in xs[i].__subclasses__():
            if cls not in seen:  # <FIXED: diamond inheritance
                add(cls)
                append(cls)
        i += 1

    cache[root_cls] = xs
    return xs
