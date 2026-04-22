import os
from typing import TYPE_CHECKING

# HACK: good for exploratory programming -- to ensure autocomplete without running stubgen every 5 minutes
if TYPE_CHECKING:
    # This block is for IDEs/Autocomplete only.
    # We point it to the python version as the "source of truth".
    from miurcache_py import *
else:
    match os.getenv("MIURCACHE_BACKEND", "auto"):
        case "cx":
            from miurcache_cx import *
        case "py":
            from miurcache_py import *
        case "auto":
            try:
                from miurcache_cx import *
            except ImportError:
                from miurcache_py import *
        case _ as err:
            raise ValueError(err)
