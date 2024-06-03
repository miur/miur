#!/usr/bin/python -SIB
#!/usr/bin/env -S python -SIB -Ximporttime
#!/usr/bin/env -S python -SIB -m cProfile -s cumulative --
#   -I(-P/s/E) -B | -O -X pycache_prefix=~/.cache/miur
# PERF:DEBUG: $ PYTHONPATH=/d/just python -PsSB [-X importtime | -X tracemalloc] -m miur
#   OR: $ python -m cProfile -s cumulative -- =miur
#   [_] TODO: run all of them through flags
#     e.g. set flags=noninteractive (to exit fast) and then exec(cmdline)
#   [_] ALSO: directly run Jupyther kernel and console by sourcing from inside my code
#     /d/coastline/fleur/cfg/gdb-jupyter.py
# %SUMMARY: frontend
# %USAGE: $ mi || miur || . =mi
"""":
this=$(realpath -e "${BASH_SOURCE[0]:-$0}")
[[ ${this%/*/*} -ef /d/miur ]] && this=/d/miur/${this#${this%/*/*}/}
if (return 0 2>/dev/null); then
    _ps4=$PS4 && PS4=' ' && set -x

    alias miur.pkg="${this%/*/*}/pkg/PKGBUILD.dev"
    alias miur.impt="python -SIB -Ximporttime -- $this"
    alias miur.prof="python -SIB -m cProfile -s cumulative -- $this"

    # alias mi="$this"
    alias ma='miur -a'
    alias ml='miur -a'
    alias mK='miur -K'
    alias mI='miur -I'

    if [[ ${ZSH_NAME:+x} ]]; then
        alias -g M='|miur'
    fi

    set +x && PS4=$_ps4
    unset this _ps4
    return 0
fi
set -o errexit -o errtrace -o noclobber -o noglob -o nounset -o pipefail
# exec "$@"
echo "ERR: '$0' is not supposed to be run by $SHELL"
exit -2
"""


import sys


def select_entrypoint():  # type:ignore[no-untyped-def]
    # argv = sys.argv[1:sys.argv.index('--')] if '--' in sys.argv else sys.argv[1:]
    # sys.dont_write_bytecode = '-c' in argv or '--clean' in argv
    argv = sys.argv
    # PERF: faster startup w/o importing ArgumentParser (128ms vs 115ms)
    if len(argv) == 1 or (len(argv) > 1 and argv[1] == "--"):
        from .miur import miur_main

        return miur_main

    from .cli import miur_argparse

    return lambda: miur_argparse(argv)


def as_pkg_or_exe(mkrun):  # type:ignore[no-untyped-def]
    sys.path = [p for p in sys.path if not p.endswith(".zip")]
    if globals().get("__package__") is not None:
        return mkrun()

    ## HACK:PERF:(-3ms): don't import os.path on POSIX during DEV
    ##   BAD: "curses" imports "os" anyways
    ##   BUT: we won't need "os" for cli/headless pure graph processor,
    ##     so this performance hack is still feasible
    if sys.path and sys.path[0][0] == "/":
        # FAIL: we use symlink
        # parent = __file__.rsplit('/')[0]
        parent = "/d/miur"
    else:
        # OR: fs = __import__("os.path", fromlist=[""])
        import os.path as fs

        parent = fs.dirname(fs.dirname(fs.realpath(__file__)))

    sys.path.insert(0, parent)

    ## OR:BET? main = __import__("importlib").import_module(".cli", package="miur").main
    # pylint:disable=global-statement,redefined-builtin
    global __package__
    __package__ = "src"
    try:
        return mkrun()
    finally:
        __package__ = None  # type:ignore[assignment]
        sys.path.remove(parent)


sys.exit(as_pkg_or_exe(select_entrypoint)())  # type:ignore[no-untyped-call]
