import os
import sys
from io import TextIOWrapper
from typing import Any, Final, TextIO

# NOTE: hardcoded inside underlying ncurses C-lib
CURSES_STDIN_FD: Final = 0
CURSES_STDOUT_FD: Final = 1
CURSES_STDERR_FD: Final = 2


class redir_stdio_nm:
    _fdini: int
    _ttyio: TextIO
    _conf: dict[str, Any]

    # CHG:(open): allow passing from outside:
    #   * path to be opened with same flags (like currently TTY)
    #   * already opened fd-r/w
    #   * already opened single fd-rw
    def __init__(self, nm: str) -> None:  # ALT: Literal["stdin", "stdout", "stderr"]
        assert nm in ("stdin", "stdout", "stderr"), nm
        self._stdnm = nm
        self._conf = {}

    def __enter__(self) -> TextIO:
        nm = self._stdnm  # OR: nm -> stdio.name[1:-1]
        stdio = getattr(sys, nm)
        assert stdio is getattr(sys, "__" + nm + "__"), "IPython limitation"
        fdini = stdio.fileno()
        assert fdini == globals()[f"CURSES_{nm.upper()}_FD"], "Sanity check"
        for attr in ("line_buffering", "write_through", "encoding"):
            self._conf[attr] = getattr(stdio, attr)
        fddup = os.dup(fdini)
        os.set_inheritable(fddup, False)
        dupio = os.fdopen(fddup, stdio.mode, encoding=stdio.encoding)
        assert isinstance(dupio, TextIOWrapper)
        # MAYBE:(TextIOWrapper.write_through=True): to avoid buffering before piping data
        #   PERF:OPT: allow to change buffering (depending on later measured performance)
        dupio.reconfigure(line_buffering=True, write_through=True)
        setattr(sys, nm, dupio)
        setattr(sys, "__" + nm + "__", dupio)
        # WARN: we can't keep it as "old_stdio" to directly restore on __exit__()
        #   COS: os.dup2() will close current underlying fd, making stdio disfunctional
        stdio.close()

        # ATT: we can't reuse "stdio.mode" -- it will be wrong in case of pipe/redir
        #   ! we can't reuse even ".encoding" if redirection was binary inof text
        mode = "r" if nm == "stdin" else "w"
        # OR:DFL: encoding=locale.getencoding() | "utf-8"
        # pylint:disable=consider-using-with
        self._ttyio = open("/dev/tty", mode, encoding="locale")
        # NOTE: fflush is ignored on STDIN
        #   REF: https://c-faq.com/stdio/stdinflush.html , https://c-faq.com/stdio/stdinflush2.html
        # OR: libc = ctypes.CDLL(None); c_stdio = ctypes.c_void_p.in_dll(libc, nm); libc.fflush(c_stdio)
        os.fsync(fdini)
        # RQ:(inheritable=True): we need FD bound to TTY for shell_out() to work
        os.dup2(self._ttyio.fileno(), fdini, inheritable=True)
        self._fdini = fdini
        return dupio

    def __exit__(self, et=None, exc=None, tb=None):  # type:ignore[no-untyped-def]
        nm = self._stdnm
        dupio = getattr(sys, nm)
        assert dupio is getattr(sys, "__" + nm + "__"), "IPython limitation"
        fddup = dupio.fileno()
        assert fddup != globals()[f"CURSES_{nm.upper()}_FD"], "Sanity check"
        fdini = self._fdini
        os.fsync(fdini)
        os.dup2(fddup, fdini, inheritable=True)
        self._ttyio.close()
        # FIXED(closefd=False):ERR:(on exit):
        #   sys:1: ResourceWarning: unclosed file <_io.TextIOWrapper name=0 mode='r' encoding='utf-8'>
        cf = self._conf
        stdio = os.fdopen(fdini, dupio.mode, closefd=False)
        assert isinstance(stdio, TextIOWrapper)
        stdio.reconfigure(
            encoding=cf["encoding"],
            line_buffering=cf["line_buffering"],
            write_through=cf["write_through"],
        )
        setattr(sys, nm, stdio)
        setattr(sys, "__" + nm + "__", stdio)
        dupio.close()
