from typing import TYPE_CHECKING, TextIO

if TYPE_CHECKING:
    import _curses as C


class AppIO:
    redirin: TextIO | None = None
    redirout: TextIO | None = None
    ttyin: TextIO | None = None  # !fd=0
    ttyout: TextIO | None = None  # !fd=1
    mixedout: TextIO | None = None  # -> ttyout
    logsout: TextIO | None = None  # -> mixedout


# RENAME? Context
class Application:
    stdscr: "C.window"
    io: AppIO = AppIO()
    # ttyattached: Task


app = Application()
