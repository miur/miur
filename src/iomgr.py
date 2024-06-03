import os
import sys

if globals().get("TYPE_CHECKING"):
    from typing import Final, TextIO  # , BinaryIO

    from .app import AppGlobals


# NOTE: hardcoded inside underlying ncurses C-lib
CURSES_STDIN_FD: "Final" = 0
CURSES_STDOUT_FD: "Final" = 1
CURSES_STDERR_FD: "Final" = 2


# MAYBE: allow BytesIO too (by IO[Any]) -- to pipe .json.zip or custom binary protocols
def dup_and_close_stdio(stdio: "TextIO") -> "TextIO":
    nm = stdio.name[1:-1]
    __stdio = getattr(sys, "__" + nm + "__")
    # MAYBE? move asserts outside into init_explicit_io() branch logic
    assert stdio is __stdio, "IPython limitation"
    fddup = os.dup(stdio.fileno())
    os.set_inheritable(fddup, False)  # for Windows
    # PERF:OPT: set explicit .buffering and .encoding (with DFL: encoding="locale")
    enc, lbuf, wrth = stdio.encoding, stdio.line_buffering, stdio.write_through
    dupio: "TextIO" = os.fdopen(fddup, stdio.mode, encoding=enc)
    # OR: fdopen(..., buffering=0(BinaryIO) | 1(TextIO) | >1(Chunks))
    dupio.reconfigure(line_buffering=lbuf, write_through=wrth)
    # WARN: we can't keep it as "old_stdio" to directly restore through e.g. dupio.close()
    #   COS: os.dup2() will close current underlying fd, making stdio disfunctional
    stdio.close()  # INFO: also does .flush() inside
    assert __stdio.closed
    return dupio


def tty_open_onto_fd(fd: int) -> "TextIO":
    # ATT: we can't reuse "stdio.mode" -- it will be wrong in case of pipe/redir
    #   ! we can't reuse even ".encoding" if redirection was binary inof text
    # OR:DFL: encoding=locale.getencoding() | "utf-8"
    # pylint:disable=consider-using-with
    ttyio = open("/dev/tty", ("r" if fd == 0 else "w"), encoding="locale")
    # NOTE: fflush is ignored on STDIN
    #   REF: https://c-faq.com/stdio/stdinflush.html , https://c-faq.com/stdio/stdinflush2.html
    # OR: libc = ctypes.CDLL(None); c_stdio = ctypes.c_void_p.in_dll(libc, nm); libc.fflush(c_stdio)
    os.fsync(fd)
    # RQ:(inheritable=True): we need FD bound to TTY for shell_out() to work
    #   BET: do shell_out() with explicit FD
    os.dup2(ttyio.fileno(), fd, inheritable=True)
    return ttyio


# WARN: all default "sys.stdin/out/err" will become *disfunctional* after this call
#   e.g. set ".quiet=1" to prevent Jupyter::kernel.outstream_class(echo=sys.stdout)
# ATT: no sense to *restore* FD schema on exit -- it's a global property of App itself
def init_explicit_io(g: "AppGlobals") -> None:
    io = g.io

    # TBD? only reassing .pipein/out FD if using curses (or interactive TTY cli)
    # DFL?(no-redir): .pipein=None .pipeout/err=altscreen|ringbuffer ?
    #   TODO:OPT: explicit choice for FD dst

    def _scope_in(cin: "TextIO", fd0: int) -> None:
        assert cin.fileno() == fd0, "Sanity check"
        if cin.isatty():
            io.pipein = None
            io.ttyin = cin
        else:
            io.pipein = dup_and_close_stdio(cin)
            io.ttyin = tty_open_onto_fd(fd0)

    def _scope_out(cout: "TextIO", fd1: int) -> None:
        assert cout.fileno() == fd1, "Sanity check"
        if cout.isatty():
            io.pipeout = None
            io.ttyout = cout
        else:
            io.pipeout = dup_and_close_stdio(cout)
            io.ttyout = tty_open_onto_fd(fd1)

    def _scope_err(cerr: "TextIO", fd2: int) -> None:
        assert cerr.fileno() == fd2, "Sanity check"
        if cerr.isatty():
            io.pipeerr = None
            # FIXME? open os.pipe to cvt libc.stderr into py.log inof spitting over TTY
            #   FAIL: !deadlock! if underlying native C code fills os.pipe internal buffers
            io.ttyalt = __import__("io").StringIO()
        else:
            io.pipeerr = dup_and_close_stdio(cerr)
            io.ttyalt = None

    _scope_in(sys.stdin, CURSES_STDIN_FD)
    _scope_out(sys.stdout, CURSES_STDOUT_FD)
    _scope_err(sys.stderr, CURSES_STDERR_FD)

    ## WARN: logs in ringbuffer should be kept as-is (as `Events)
    ##   >> they shouldn't be rasterized until you know if DST is plain text, or json, or whatever

    # TBD! only go through ttyalt if using curses (or interactive TTY cli)
    if o := g.opts.logredir:
        # TBD? close on exit to avoid `ResourceWarning
        if isinstance(o, int):
            io.logsout = os.fdopen(o, "w", encoding="locale")
        elif isinstance(o, str):
            # ALSO:DEV: generic support for other redir schemas
            #   TODO: ... | file:///path/to/log?mode=a+&buffering=1 | (fifo|socket)://...
            #   DFL=ringbuf
            #   ALSO: null / disable -- optimize code
            io.logsout = open(o, "w", encoding="locale")
        else:
            raise NotImplementedError
    else:
        if io.ttyalt is None:
            io.ttyalt = __import__("io").StringIO()
        io.logsout = io.ttyalt  # BAD: possible double-close for same FD

        from .util.logger import log

        # [_] CHECK:WTF:BUG: why logs are printed on TTY even after previous sys.stdout.close()
        log.write = io.logsout.write  # TBD? restore back on scope ?

    # [_] FIXME: add hooks individually
    # BAD: { mi | cat } won't enable altscreen, and !cat will spit all over TTY
    #   ALSO: even if it's not !cat, pipeline still may eventually print something to TTY
    #   WKRND: always use altscreen unless redir to file/socket (until PERF measurements)
    #     BAD it won't help if other app produces output at different timings
    ## TEMP:DISABLED: due to kernel.outstream_class "echo"
    ##   TRY: wrap only "echo" __std*__.write
    # import os
    # import stat
    #
    # for ttyio in (sys.stdout, sys.stderr):
    #     if ttyio.isatty() or os.fstat(ttyio.fileno()).st_mode & stat.S_IFIFO:
    #         do(CE.stdio_to_altscreen(g.stdscr, ttyio))
