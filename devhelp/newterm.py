import os
import sys
import time
from contextlib import contextmanager
from subprocess import Popen
from typing import Iterator, TextIO, cast


def get_ttynm(ttynm: str | None = None) -> str:
    if ttynm is None:
        ttynm = "/dev/tty"
        # ttynm = "/dev/pts/6"
    if os.path.exists(ttynm):
        return ttynm
    ## FAIL: if all fd* are redirected $ <input just miur navi &> output
    STDIOS = (sys.stdin, sys.stdout, sys.stderr)
    return next(os.ttyname(f.fileno()) for f in STDIOS if f.isatty())


def open_tty(ttynm: str) -> tuple[TextIO, TextIO]:
    f0, f1 = sys.stdin, sys.stdout
    # pylint:disable=consider-using-with,no-member
    rtty = cast(TextIO, open(ttynm, f0.mode, encoding=f0.encoding))
    # FAIL:(f1.mode): Jupyter|AttributeError: 'ipykernel.iostream.OutStream' object has no attribute 'mode'
    wtty = cast(TextIO, open(ttynm, "w", encoding=f1.encoding))
    return rtty, wtty


@contextmanager
def newtermwindow() -> Iterator[tuple[TextIO, TextIO]]:
    # TEMP:HACK: spawn new terminal to prevent crashing/corrupting any existing ones
    #   TRY: os.getpty()
    # NOTE: use tempfile() to auto-delete opened file on .close()
    tmpf = __import__("tempfile").NamedTemporaryFile(mode='r')
    sh_script = 'tty > "$0"; inotifywait -qq -e delete_self "$0"'  # OR: "sleep 1d" / wait SIGUSR1
    bgtty = Popen(["st", "-M", "sh", "-c", sh_script, tmpf.name])
    # MAYBE: replace by asyncinotify (on file write+close) OR loop until non-empty
    time.sleep(0.5)
    ttynm = tmpf.read().strip()
    # HACK: replace jupyter/ipykernel hardcoded TERM for proper curses init
    os.environ["TERM"] = "st-256color"
    try:
        # WKRND: keep tty open until app exit (for simplicity)
        rtty, wtty = open_tty(ttynm)
        yield (rtty, wtty)
    finally:
        rtty.close()
        wtty.close()
        tmpf.close()  # OR: bgtty.terminate()
        bgtty.wait()
        print("[offterm]")
