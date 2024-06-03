# PERF:(typing): abc collections copyreg contextlib functools operator re sys types warnings _typing
#   NOTE: native "from io import TextIOWrapper" is much faster
if globals().get("TYPE_CHECKING"):
    from types import ModuleType
    from typing import TextIO

    import _curses as C


# BET: don't reassing cmdline opts -- treat them as Final, and SEP from active "AppState"
#   e.g. use typing.NamedTuple -- and assign all options at once BUT:BAD: still need DFL
# termcolor = sys.stdout.isatty()
class AppOptions:
    # USAGE: time mi --backend=asyncio
    PROFILE_STARTUP = False  # =DEBUG
    ####
    bare: bool = True
    ipykernel: bool = False
    ipyconsole: bool = False
    color: bool | None
    ####
    cwd: str
    ####
    signal: int | None


# ATT: use these FD explicitly: don't ever use "sys.std{in,out,err}"
class AppIO:
    pipein: "TextIO | None" = None
    pipeout: "TextIO | None" = None
    pipeerr: "TextIO | None" = None
    ttyin: "TextIO | None" = None  # !fd=0
    ttyout: "TextIO | None" = None  # !fd=1
    mixedout: "TextIO | None" = None  # -> ttyout
    logsout: "TextIO | None" = None  # -> mixedout


# class AppState:
#     ttyattached: Task


# FUT:RENAME? c = g_ctx = AppContext() | ns = AppNamespace()
#   &why: so we won't confuse multiple separate apps contexts (server,clients) with single global state
class AppGlobals:
    _main: "ModuleType"
    stdscr: "C.window"
    io = AppIO()
    opts = AppOptions()


g_app = AppGlobals()
# g = g_app
