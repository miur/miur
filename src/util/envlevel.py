import os
import sys
from contextlib import contextmanager
from typing import Iterator

from .logger import log


@contextmanager
def increment_envlevel(varname: str) -> Iterator[int]:
    lvl = int(os.environ.get(varname, "0"))
    if lvl != 0:
        log.error(f"avoid nesting {lvl=}")
        sys.exit(1)
    os.environ[varname] = str(lvl + 1)
    try:
        yield lvl
    finally:
        os.environ[varname] = str(lvl)


@contextmanager
def save_choosedir(path: str) -> Iterator[None]:
    try:
        yield
    finally:
        from ..app import g_app

        loci = g_app.root_wdg._navi._view._ent.loci
        log.state(f"cwd={loci}")
        with open(path, "w", encoding="utf-8") as f:
            f.write(loci)
