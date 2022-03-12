import os
import sys
from contextlib import contextmanager
from typing import Iterator, TextIO


@contextmanager
def redir_fd(fd: int, tgt: int) -> Iterator[int]:
    # WARN: stdin/stdout cannot be reopened (with standard C means) once it's closed
    #     and must be duplicated beforehand if you still need it.
    #   SRC: https://stackoverflow.com/questions/40826895/reopening-sys-stdout-after-it-is-closed-by-with-statement
    #   SRC: https://stackoverflow.com/questions/9084099/re-opening-stdout-and-stdin-file-descriptors-after-closing-them
    temp = os.dup(tgt)
    try:
        # WARN: dup2 closes previous tgt -- which makes "sys.stdout" object invalid!
        #   MAYBE:NEED:(here): sys.stdout.close() OR:CHECK: .close() works afterwards
        #   BAD: we don't know (and can't close) if user already duplicated 0/1 into some local objects
        os.dup2(fd, tgt)
        yield temp
    finally:
        os.dup2(temp, tgt)
        # BUG! on restore temp0 will be closed -- making new sys.stdin invalid
        os.close(temp)


## NOTE: should reassign sys.* fds -- prevent !python access to newterm
@contextmanager
def safe_reassign_sysio(nm: str, fdwas: int, fduse: int) -> Iterator[tuple[bool, bool]]:
    stdio = getattr(sys, nm)
    bkpio = getattr(sys, "__" + nm + "__")
    seq = stdio.fileno() == fdwas
    beq = bkpio.fileno() == fdwas
    mode = None

    # ATT:(outside try-catch): don't reassign STDIO if .close raised BadDescr
    if seq:
        mode = stdio.mode
        stdio.close()
    # FIXED: closed twice when "sys.stdin == sys.__stdin__"
    if beq and not seq:
        mode = bkpio.mode
        bkpio.close()

    try:
        yield (seq, beq)
    finally:
        # FIXED: on restore "fdtmp" will be closed -- making new sys.stdin invalid
        # FIXED:BUG:(BadDescr): if .fdopen twice -- and then .close both
        if seq or beq:
            assert mode is not None
            newio = os.fdopen(fduse, mode)
            if seq:
                setattr(sys, nm, newio)
            if beq:
                setattr(sys, "__" + nm + "__", newio)


@contextmanager
def rebind_sysio_to_fd(nm: str, fdold: int, fdnew: int) -> Iterator[int]:
    fdtmp = os.dup(fdold)
    try:
        # ATT:(outside try-catch): propagate BadDescr from .close w/o restoring
        with safe_reassign_sysio(nm, fdold, fdtmp):
            # WARN: low-level Jupyter may break when we try to redirect "remote output"
            #   << COS: it may have stored copies of sys.std* somewhere
            os.dup2(fdnew, fdold)
    except:  # pylint:disable=bare-except
        os.close(fdtmp)

    try:
        yield fdtmp
    finally:
        # WARN: after ctxm "fdtmp" becomes BadDescr due to "sys.stdin.close()"
        os.fstat(fdtmp)
        os.fstat(fdold)
        os.dup2(fdtmp, fdold)
        with safe_reassign_sysio(nm, fdtmp, fdold):
            pass


## WKRND:BAD: libncurses.so initscr() hardcodes fd0 and fd1
#    //base/lib_initscr.c:93 if (newterm(name, stdout, stdin) == 0)
#    :: must redirect fd1 even if passing "ttyfd" into setupterm()
# [$] MAYBE:HACK: reassign 0/1 only during initscr()
#   FAIL:ERR:(write): we should keep 0/1 redirected until !curses exit
## DISABLED:(sys.stderr): keep newterm stderr inside Jupyter too
@contextmanager
def bind_fd01_from_tty(rtty: TextIO, wtty: TextIO) -> Iterator[tuple[int, int]]:
    with (
        rebind_sysio_to_fd("stdin", 0, rtty.fileno()) as temp0,
        rebind_sysio_to_fd("stdout", 1, wtty.fileno()) as temp1,
    ):
        ## DEBUG
        # print(
        #     f"""
        #     {rtty.fileno()=} {wtty.fileno()=}
        #     {temp0=} {temp1=}
        #     {sys.stdin.fileno()=} {sys.stdout.fileno()=}
        #     {sys.__stdin__.fileno()=} {sys.__stdout__.fileno()=}
        # """
        # )
        yield (temp0, temp1)
