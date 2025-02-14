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
def save_choosedir(turl: str, tcwd: str) -> Iterator[None]:
    try:
        yield
    finally:
        from ..app import g_app

        ## TODO: discard non-FS nodes from miur://URI
        #   --choosedir simplify path to at most dir
        #   --choosefile should only allow filenames at the end, but not dirs
        #   --choosefileline should allow "file:33:12" suffix from lines navigation
        #   --chooseany/--chooseitem should allow any format flexibly by whatever under cursor
        if turl:
            url = g_app.root_wdg._navi._view._ent.loci
            log.state(f"url={url}")
            with open(turl, "w", encoding="utf-8") as f:
                f.write(url)
        if tcwd:
            cwd = os.getcwd()
            log.state(f"cwd={cwd}")
            with open(tcwd, "w", encoding="utf-8") as f:
                f.write(cwd)
