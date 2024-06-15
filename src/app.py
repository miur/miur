# PERF:(typing): abc collections copyreg contextlib functools operator re sys types warnings _typing
#   NOTE: native "from io import TextIOWrapper" is much faster
if globals().get("TYPE_CHECKING"):
    from io import StringIO
    from types import ModuleType
    from typing import Callable, Optional, TextIO, Union

    import _curses as C


# BET: don't reassing cmdline opts -- treat them as Final, and SEP from active "AppState"
#   e.g. use typing.NamedTuple -- and assign all options at once BUT:BAD: still need DFL
# [_] TODO? allow dict['some-opt'] to access non-keyword cmdline-compatible options
# termcolor = sys.stdout.isatty()
class AppOptions:
    # USAGE: time mi --backend=asyncio
    PROFILE_STARTUP = False  # =DEBUG
    ####
    bare: bool = True
    ipykernel: bool = False
    ipyconsole: bool | None = None
    ####
    color: bool | None
    # VIZ(logredir): altscreen | fd=3 | ./log | file:///path/to/log:buffering=1 | (fifo|socket)://...
    logredir: int | str | None = None
    ####
    cwd: str
    ####
    signal: int | None


# ATT: use these FD explicitly: don't ever use "sys.std{in,out,err}"
class AppIO:
    pipein: "Optional[TextIO]" = None
    pipeout: "Optional[TextIO]" = None
    pipeerr: "Optional[TextIO]" = None
    ttyin: "Optional[TextIO]" = None  # !fd=0
    ttyout: "Optional[TextIO]" = None  # !fd=1
    ttyalt: "Optional[StringIO]" = None  # -> ttyout
    logsout: "Optional[Union[StringIO,TextIO]]" = None  # -> ttyalt | pipeerr


# class AppState:
#     ttyattached: Task


class AppCursesUI:
    resize: "Callable[[], None]"
    handle_input: "Callable[[], None]"


# FUT:RENAME? c = g_ctx = AppContext() | ns = AppNamespace()
#   &why: so we won't confuse multiple separate apps contexts (server,clients) with single global state
class AppGlobals:
    _main: "ModuleType"
    stdscr: "C.window"
    io = AppIO()
    opts = AppOptions()
    curses_ui: AppCursesUI


g_app = AppGlobals()
# g = g_app
