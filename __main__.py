#!/usr/bin/env -S python -IB
# PERF:DEBUG: $ PYTHONPATH=/d/just python -PsSB [-X importtime | -X tracemalloc] -m miur
# -I(-P/s/S/E) -B | -O -X pycache_prefix=~/.cache/miur

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
    if globals()["__package__"] is not None:
        return mkrun()

    # OR: fs = __import__("os.path", fromlist=[""])
    from os.path import dirname

    parent = dirname(dirname(__file__))
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
