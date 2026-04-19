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
def save_choosedir(rhist: str, rurl: str, rcwd: str) -> Iterator[None]:
    try:
        yield
    finally:
        from ..app import g_app

        # pylint:disable=protected-access
        if rhist:
            hist = g_app.root_wdg._navi._hist
            log.state(f"rhist={hist!s}")
            with open(rhist, "w", encoding="utf-8") as f:
                f.write(hist.dump())
        if rurl:
            url = g_app.root_wdg._navi._view._ent.loci
            log.state(f"rurl={url}")
            with open(rurl, "w", encoding="utf-8") as f:
                f.write(url)
        ## TODO: discard non-FS nodes from miur://URI
        #   --choosedir simplify path to at most dir
        #   --choosefile should only allow filenames at the end, but not dirs
        #   --choosefileline should allow "file:33:12" suffix from lines navigation
        #   --chooseany/--chooseitem should allow any format flexibly by whatever under cursor
        if rcwd:
            cwd = os.getcwd()
            log.state(f"rcwd={cwd}")
            with open(rcwd, "w", encoding="utf-8") as f:
                f.write(cwd)
