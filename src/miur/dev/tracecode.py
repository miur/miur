import linecache
import sys
from collections.abc import Callable
from types import FrameType
from typing import Any

from .. import PKGSRC_DIR, log

# OR:DEBUG: $ python -m trace --trace your_script.py
TRACE_SKIP = ("logsystem", "ansicolor")


# TBD:CHG:BET:PERF: sys.monitoring() SEE &/00_trace_monitoring.py
def trace_lines(
    frame: FrameType, event: str, _arg: object
) -> Callable[..., Any] | None:
    # VIZ: ["call", "line", "return", "exception", "opcode"]
    if event == "call":
        modnm: str = frame.f_globals.get("__name__", "")
        if modnm and not modnm.startswith("miur."):
            return None
        for skipnm in TRACE_SKIP:
            if "." + skipnm + "." in "." + modnm + ".":
                return None
        ## Safeguard - Skip any file located in the standard library
        filename: str = frame.f_code.co_filename
        if "lib/python" in filename.replace("\\", "/"):
            return None
    elif event == "line":
        filename = frame.f_code.co_filename
        line_no = frame.f_lineno
        line_text = linecache.getline(filename, line_no).strip()
        if filename.startswith("<"):
            modnm = frame.f_globals.get("__name__", "???")
            fnm = modnm
        else:
            fnm = "…/" + filename.removeprefix(PKGSRC_DIR)
        log("T", line_text, loci=fnm, lnum=line_no)
    # NOTE: on 'call' -- return either ref to itself, or another function for new scope,
    #   or None avoid tracing that scope
    return trace_lines


def enable_tracelines(yes: bool = True, /) -> None:
    sys.settrace(trace_lines if yes else None)


# MAYBE? sys.call_tracing(func, args)
# from functools import wraps
# def trace_this(func: Callable[P, R]) -> Callable[P, R]:
#     @wraps(func)
#     def wrapper(*args: P.args, **kwargs: P.kwargs) -> R:
#         sys.settrace(trace_lines)
#         try:
#             return func(*args, **kwargs)
#         finally:
#             sys.settrace(None)
#     return wrapper
