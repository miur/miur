# PERF:(imports): abc collections copyreg contextlib functools operator re sys types warnings _typing
from typing import TYPE_CHECKING, TextIO

if TYPE_CHECKING:
    from types import ModuleType

    import _curses as C


# BET: don't reassing cmdline opts -- treat them as Final, and SEP from active "AppState"
#   e.g. use typing.NamedTuple -- and assign all options at once BUT:BAD: still need DFL
# termcolor = sys.stdout.isatty()
class AppOptions:
    # USAGE: time mi --backend=asyncio
    PROFILE_STARTUP = False  # =DEBUG
    ####
    bare: bool = False
    ipykernel: bool = False
    ipyconsole: bool = False
    color: bool | None
    ####
    cwd: str
    ####
    signal: int | None


class AppIO:
    redirin: TextIO | None = None
    redirout: TextIO | None = None
    ttyin: TextIO | None = None  # !fd=0
    ttyout: TextIO | None = None  # !fd=1
    mixedout: TextIO | None = None  # -> ttyout
    logsout: TextIO | None = None  # -> mixedout


# class AppState:
#     ttyattached: Task


# FUT:RENAME? c = g_ctx = AppContext() | ns = AppNamespace()
#   &why: so we won't confuse multiple separate apps contexts (server,clients) with single global state
class AppGlobals:
    _main: "ModuleType"
    stdscr: "C.window"
    io = AppIO()
    opts = AppOptions()


g = g_app = AppGlobals()