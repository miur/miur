import io
import os
import sys
from io import TextIOWrapper
from typing import TextIO, cast


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
    # ONELINE: stdout_copy = os.fdopen(os.dup(sys.stdout.fileno()))
    fd = oldf.fileno()
    fdcopy = os.dup(fd)
    os.dup2(ttyf.fileno(), fd)
    # except OSError: on wrong mode or /dev/null
    newf = cast(TextIOWrapper, os.fdopen(fdcopy, mode))
    return newf
