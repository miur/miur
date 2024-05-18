#!/usr/bin/python -SIB
#!/usr/bin/env -S python -SIBO -Ximporttime
#   -I(-P/s/E) -B | -O -X pycache_prefix=~/.cache/miur
# PERF:DEBUG: $ PYTHONPATH=/d/just python -PsSB [-X importtime | -X tracemalloc] -m miur
#   OR: $ python -m cProfile -s cumulative -- =miur
#   [_] TODO: run all of them through flags
#     e.g. set flags=noninteractive (to exit fast) and then exec(cmdline)
#   [_] ALSO: directly run Jupyther kernel and console by sourcing from inside my code
#     /d/coastline/fleur/cfg/gdb-jupyter.py

import sys


def select_entrypoint():  # type:ignore
    argv = sys.argv
    # PERF: faster startup w/o importing ArgumentParser (128ms vs 115ms)
    if len(argv) > 1 and argv[1] == "--":
        from .miur import miur_none

        return miur_none

    from .cli import miur_args

    return lambda: miur_args(argv[1:])


def as_pkg_or_exe(mkrun):  # type:ignore
    if globals().get("__package__") is not None:
        return mkrun()

    # OR: fs = __import__("os.path", fromlist=[""])
    import os.path as fs

    parent = fs.dirname(fs.dirname(fs.realpath(__file__)))
    sys.path.insert(0, parent)

    ## OR:BET? main = __import__("importlib").import_module(".cli", package="miur").main
    # pylint:disable=global-statement,redefined-builtin
    global __package__
    __package__ = "miur"
    try:
        return mkrun()
    finally:
        __package__ = None  # type:ignore
        sys.path.remove(parent)


sys.exit(as_pkg_or_exe(select_entrypoint)())  # type:ignore
