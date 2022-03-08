import curses as C
import io
import os
import sys
import time
from contextlib import contextmanager
from io import TextIOWrapper
from subprocess import Popen
from typing import Any, TextIO, cast

## HACK
# try:
#     __IPYTHON__
# except NameError:
#     __IPYTHON__ = False
# if __IPYTHON__:
#     pass


# ALT: Redirect stdout to a file in Python? - Stack Overflow ⌇⡡⣚⣾⠡
#   https://stackoverflow.com/questions/4675728/redirect-stdout-to-a-file-in-python/22434262#22434262
# [_] CMP: /@/research/airy/pacman/pikaur/pikaur/pikspect.py:87: class TTYInputWrapper():
#   SEE: prompt-toolkit and fzf -- how to make a "partial" screen with ncurses ifc
# stdout - How to I make python curses application pipeline friendly? - Stack Overflow ⌇⡡⢬⡺⠄
#   https://stackoverflow.com/questions/53696818/how-to-i-make-python-curses-application-pipeline-friendly
# percol/percol at master · mooz/percol ⌇⡡⢬⡹⣵
#   https://github.com/mooz/percol/tree/master/percol
def DEPR_rebind_stdios_to_tty() -> dict[str, TextIO]:
    tty = {}
    odummy = io.StringIO()
    for f in (sys.stdin, sys.stdout):  # FAIL:(sys.stderr): not collected
        # HACK: bind initial "fd0" to "tty" for curses and assign new "fd" to old stdin
        nm = f.name[1:-1]
        if f.isatty():
            # NOTE: redirect stdin/stdout even if it's already "tty"
            # HACK: ignore STDIN but collect merged(STDOUT/STDERR) to print on __exit__
            ftty = open("/dev/tty", f.mode, encoding=f.encoding)
            os.dup2(ftty.fileno(), f.fileno())
            newf = open(os.devnull, f.mode) if f.mode == "r" else odummy
        else:
            oldfd = os.dup(f.fileno())
            ftty = open("/dev/tty", f.mode, encoding=f.encoding)
            os.dup2(ftty.fileno(), f.fileno())
            try:
                newf = os.fdopen(oldfd, f.mode)
            except OSError:  # wrong mode or /dev/null
                # oldfd = None
                raise
        # HACK: override high-level objects by dups and abandon low-level fileno
        setattr(sys, nm, newf)
        setattr(sys, "__" + nm + "__", newf)
        tty[nm] = ftty
    return tty


def get_ttynm(ttynm: str | None = None) -> str:
    if ttynm is None:
        ttynm = "/dev/tty"
        # ttynm = "/dev/pts/6"
    if os.path.exists(ttynm):
        return ttynm
    ## FAIL: if all fd* are redirected $ <input just miur navi &> output
    STDIOS = (sys.stdin, sys.stdout, sys.stderr)
    return next(os.ttyname(f.fileno()) for f in STDIOS if f.isatty())


def open_tty(ttynm: str | None = None) -> tuple[TextIO, TextIO]:
    if ttynm is None:
        ttynm = get_ttynm()
    f0, f1 = sys.stdin, sys.stdout
    # pylint:disable=consider-using-with,no-member
    rtty = cast(TextIO, open(ttynm, f0.mode, encoding=f0.encoding))
    # FAIL:(f1.mode): Jupyter|AttributeError: 'ipykernel.iostream.OutStream' object has no attribute 'mode'
    wtty = cast(TextIO, open(ttynm, "w", encoding=f1.encoding))
    return rtty, wtty


def override_io(oldf: TextIO, ttyf: TextIO, mode: str = None) -> TextIOWrapper:
    # if oldf.isatty():
    #     # NOTE: redirect stdin/stdout even if it's already "tty"
    #     fd = oldf.fileno()
    #     os.dup2(ttyf.fileno(), fd)
    #     # HACK: ignore STDIN but collect merged(STDOUT/STDERR) to print on __exit__
    #     newf = open(os.devnull, f.mode) if f.mode == "r" else odummy
    # else:
    if mode is None:
        mode = oldf.mode
    fd = oldf.fileno()
    fdcopy = os.dup(fd)
    os.dup2(ttyf.fileno(), fd)
    # except OSError: on wrong mode or /dev/null
    newf = cast(TextIOWrapper, os.fdopen(fdcopy, mode))
    return newf


def rebind_stdios_to_tty(ttynm: str | None = None) -> tuple[TextIO, TextIO]:
    # WKRND: keep tty open until app exit (for simplicity)
    rtty, wtty = open_tty(ttynm)
    ### FAIL: initscr() hardcodes fd0 and fd1 WKRND:USE: redirect 0/1 directly
    ##   //base/lib_initscr.c:93 if (newterm(name, stdout, stdin) == 0)
    ## [_] FIXME: if stdin already redirected -- open or redirect 0/1 itself
    if sys.__stdin__.fileno() == 0:
        sys.__stdin__ = override_io(sys.__stdin__, rtty)
    if sys.__stdout__.fileno() == 1:
        sys.__stdout__ = override_io(sys.__stdout__, wtty, "w")
    if sys.stdin.fileno() == 0:
        sys.stdin = override_io(sys.stdin, rtty)
    if sys.stdout.fileno() == 1:
        ## WKRND:BAD: must redirect fd1 even if passing "ttyfd" into setupterm()
        # FAIL:(Jupyter): AttributeError: 'ipykernel.iostream.OutStream' object has no attribute 'mode'
        sys.stdout = override_io(sys.stdout, wtty, "w")
    ## DISABLED: keep newterm stderr inside Jupyter too
    # sys.stderr = override_io(wtty, sys.stderr)
    ## BAD: should reassign old sys.* fds for !python to prevent access to newterm
    # _fd0 = os.dup(0); os.dup2(rtty.fileno(), 0)
    # _fd1 = os.dup(1); os.dup2(wtty.fileno(), 1)
    ## FIXME:TRY: reassign stdout only temporarily until curses binds itself to TTY
    #   << FAIL? libncurses.so directly uses fd0 inside getch()
    ## WARN:FAIL: we can't restore 0/1 until !ncurses exists (OR it will crash)
    # finally: os.dup2(fd0, 0); os.dup2(fd0, 1)
    return (rtty, wtty)


@contextmanager
def newtermwindow() -> Any:
    # TEMP:HACK: spawn new terminal to prevent corrupting any existing ones
    #   TRY: os.getpty()
    bgtty = Popen(["st", "-M", "sh", "-c", "tty|tee /tmp/miurttynm; sleep 1d"])
    time.sleep(1)  # MAYBE: replace by asyncinotify
    ttynm = __import__("pathlib").Path("/tmp/miurttynm").read_text().strip()
    try:
        # FIXME:SPLIT: open_tty() ++ rebind_()
        rtty, wtty = rebind_stdios_to_tty(ttynm)
        # rtty.fileno()
        # wtty.fileno()
        # sys.stdin.fileno()
        # sys.stdout.fileno()
        # sys.__stdin__.fileno()
        # sys.__stdout__.fileno()
        yield (rtty, wtty)
    finally:
        rtty.close()
        wtty.close()
        bgtty.terminate()


class TUI:
    stdscr: C.window

    def __init__(self) -> None:
        if not C.has_extended_color_support():
            raise NotImplementedError

    @staticmethod
    def _init(*args: Any, **kw: Any) -> C.window:
        ## DISABLED:(setupterm): using it before initscr is only !python hack for errs
        # OR:(term=os.environ.get("TERM", "unknown"), fd=wtty.fileno())
        # C.setupterm(*args, **kw)
        stdscr = C.initscr()  # ALT: newterm(NULL, outfd, infd)
        C.noecho()  # echoing of keys = off
        C.cbreak()  # buffering on keyboard input = off
        stdscr.keypad(True)  # sup special escape seq for e.g. curses.KEY_LEFT
        try:
            C.start_color()  # ignorable
        except:  # pylint:disable=bare-except
            pass
        return stdscr

    def __enter__(self) -> "TUI":
        self.stdscr = self._init()
        return self

    def __exit__(self, exc_type: Any, exc_value: Any, traceback: Any) -> None:
        if self.stdscr is not None:
            self.stdscr.keypad(False)
            C.echo()
            C.nocbreak()
            C.endwin()
            # BAD: not ported :: delscreen(stdscr)

        # if isinstance(sys.stdout, io.StringIO):
        #     sys.stdout.flush()
        #     otxt = sys.stdout.getvalue()
        #     sys.stdout.close()
        #     self.tty["stdout"].write(otxt)
        # if isinstance(sys.stderr, io.StringIO):
        #     etxt = sys.stderr.getvalue()
        #     sys.stderr.close()
        #     self.tty["stderr"].write(etxt)
