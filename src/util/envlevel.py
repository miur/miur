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
