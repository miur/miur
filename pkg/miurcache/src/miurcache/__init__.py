import os
from typing import TYPE_CHECKING

# HACK: good for exploratory programming -- to ensure autocomplete without running stubgen every 5 minutes
if TYPE_CHECKING:
    # This block is for IDEs/Autocomplete only.
    # We point it to the python version as the "source of truth".
    from ._pure import *
else:
    match os.getenv("MIURCACHE_BACKEND", "auto"):
        # case "cx":  # RENAME? use actual matching impl codename inof generic cx/py designators
        case "ext_cc":
            # ERR: ImportError: dynamic module does not define module export function (PyInit__ext_cc)
            # from ._ext_cc import *
            from . import _ext_cc as impl

            hello = impl.hello
            __all__ = ["hello"]

            # NOTE: can be useful in testing
            #   BET: define "_impl_codename" inside extension itself -- to be populated by "*" on import
            # IMPL_USED = "ext_cc"
        case "pure":  # case "py":
            # RENAME? _pure _python _canonical | (_SoA vs _AoS vs _pythonic/dataclass)
            from ._pure import *

            # IMPL_USED = "pure"
        case "auto":
            try:
                # CASE: try loading the C extension first
                from ._ext_cc import *
            except ImportError:
                from ._pure import *
        case _ as err:
            raise ValueError(err)

## ALT: Expose the API to the user
# hello = impl.hello
# __all__ = ["hello", "IMPL_USED"]
