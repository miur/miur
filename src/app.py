# PERF:(typing): abc collections copyreg contextlib functools operator re sys types warnings _typing
#   NOTE: native "from io import TextIOWrapper" is much faster
if globals().get("TYPE_CHECKING"):
    from io import StringIO
    from multiprocessing import Process
    from types import ModuleType
    from typing import Callable, Optional, TextIO, Union

    import _curses as C

    from .entity.base import Golden
    from .ui.root import RootWidget


# BET: don't reassing cmdline opts -- treat them as Final, and SEP from active "AppState"
#   e.g. use typing.NamedTuple -- and assign all options at once BUT:BAD: still need DFL
# [_] TODO? allow dict['some-opt'] to access non-keyword cmdline-compatible options
# termcolor = sys.stdout.isatty()
class AppOptions:
    # USAGE: time mi --backend=asyncio
    PROFILE_STARTUP = False  # =DEBUG
    devroot: str | None = None
    ####
    bare: bool = False
    devinstall: bool = False
    ipykernel: bool = False
    ipyconsole: bool | None = None
    ####
    color: bool | None
    # VIZ(logredir): altscreen | fd=3 | ./log | file:///path/to/log:buffering=1 | (fifo|socket)://...
    logredir: int | str | None = None
    loglevel: int
    ####
    xpath: str | None
    stdinfmt: "Optional[Golden]"
    ####
    signal: int | None
    remember_url: str = None
    choosedir: str = None


# ATT: use these FD explicitly: don't ever use "sys.std{in,out,err}"
class AppIO:
    pipein: "Optional[TextIO]" = None
    pipeout: "Optional[TextIO]" = None
    pipeerr: "Optional[TextIO]" = None
    ttyin: "Optional[TextIO]" = None  # !fd=0
    ttyout: "Optional[TextIO]" = None  # !fd=1
    ttyalt: "Optional[StringIO]" = None  # -> ttyout
    logsout: "Optional[Union[StringIO,TextIO]]" = None  # -> ttyalt | pipeerr
    logfdchild: int  # back from child processes to miur (parent)
    logfdparent: int  # TEMP: pass to asyncio to listen for logs


# class AppState:
#     ttyattached: Task


class AppCursesUI:
    resize: "Callable[[], None]"
    handle_input: "Callable[[], None]"


type KeyTable = "dict[str | int, Callable[[AppGlobals], None] | KeyTable]"


# FUT:RENAME? c = g_ctx = AppContext() | ns = AppNamespace()
#   &why: so we won't confuse multiple separate apps contexts (server,clients) with single global state
class AppGlobals:
    _main: "ModuleType"
    stdscr: "C.window"
    # MAYBE:(inof doexit/exiting): directly store {ev_shutdown: asyncio.Event}.is_set()
    doexit: "Callable[[], None]"
    io = AppIO()
    opts = AppOptions()
    curses_ui: AppCursesUI
    root_wdg: "RootWidget"  # Root/FM/ListWidget
    keytable: KeyTable  # =current/cursor
    keytablename: str
    keytableroot: KeyTable  # =read-only, whole tree
    ## IMPL(exiting): change FSM operation mode
    #   - stop accepting new cmds/input-data
    #   - reduce bkgr tasks timeout to close faster
    #   - don't refresh screen beside spinner area
    exiting: bool = False
    inputfield: str = ""
    inputpos: int = -1
    mp_children: "dict[str, Process]" = {}


g_app = AppGlobals()
# g = g_app
