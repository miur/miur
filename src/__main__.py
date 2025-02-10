#!/usr/bin/env -S python -SIB -X faulthandler
#!/usr/bin/python -SIB
#!/usr/bin/env -S python -SIB -Ximporttime
#!/usr/bin/env -S python -SIB -m cProfile -s cumulative --
# ALT(-B): -O -X pycache_prefix=~/.cache/miur
#   -I(-P/s/E)
# PERF:DEBUG: $ PYTHONPATH=/d/just python -PsSB [-X importtime | -X tracemalloc] -m miur
#   OR: $ python -m cProfile -s cumulative -- =miur
#   [_] TODO: run all of them through flags
#     e.g. set flags=noninteractive (to exit fast) and then exec(cmdline)
#   [_] ALSO: directly run Jupyther kernel and console by sourcing from inside my code
#     /d/coastline/fleur/cfg/gdb-jupyter.py
# %SUMMARY: frontend
# %USAGE: $ mi || miur || . =mi
"""":
if (return 0 2>/dev/null); then
    _app=$(realpath -e "${BASH_SOURCE[0]:-$0}")
    if [[ ${_app#/usr/} != ${_app} ]]
    then _als="${_app%/*/*}/share/miur/integ/miur-shell-aliases.sh"
    else _als="${_app%/*/*}/integ/miur-shell-aliases.sh"
    fi
    source "$_als" "$_app" "$@"
    unset _app _als
    return 0
fi
echo "ERR: '$0' is not supposed to be run by $SHELL"
# exec "$0" "$@"
exit -2
"""


import sys


def select_entrypoint(devroot: str):  # type:ignore[no-untyped-def]
    # argv = sys.argv[1:sys.argv.index('--')] if '--' in sys.argv else sys.argv[1:]
    # sys.dont_write_bytecode = '-c' in argv or '--clean' in argv
    argv = sys.argv

    # HACK:(--bare): use alt-name "mi-" to run faster (but limited) raw EPOLL backend
    bare = argv[0].rpartition("/")[2] in ("mi-", "miur-")

    from .app import g_app
    from .util.devenv import get_py_args
    from .util.logger import log

    # ALT?(sys.modules[__main__]): will it work in !jupyter ?
    g_app._main = sys.modules[__name__]  # pylint:disable=protected-access
    o = g_app.opts
    o.bare = bare
    o.devroot = devroot

    # BAD: log is too early to be redirected by stdlog_redir()
    venv_path = p if (p := sys.prefix) != sys.base_prefix else "---"
    if (b := sys.base_prefix) != "/usr":
        venv_path += " ; " + b
    log.state(f"(.venv): {venv_path}")
    log.state(f"<$ {' '.join(repr(a) if ' ' in a else a for a in get_py_args())}")
    if o.PROFILE_STARTUP:
        log.kpi("entrypoint")

    # PERF: run --bare w/o .venv; NEED: install all deps by system pkg manager
    if not bare:
        from .util.devenv import ensure_venv

        # BAD: shell_out() will be inside that .venv too
        #   >> ATT: you shouldn't run *any* python programs in that nested shell
        # MAYBE: make a frontend to miur (like "fleur/ctl" did)
        #   >> move all dev-helpers there and access miur only through it
        #   &why keep only essential features in primary codebase
        ensure_venv(devroot, dev=devroot == "/d/miur")

    # PERF: faster startup w/o importing ArgumentParser (128ms vs 115ms)
    if len(argv) == 1 or (len(argv) > 1 and argv[1] == "--"):
        from .miur import miur_main

        return lambda: miur_main(g_app)

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
        return mkrun(parent)
    finally:
        __package__ = None  # type:ignore[assignment]
        sys.path.remove(parent)


sys.exit(as_pkg_or_exe(select_entrypoint)())  # type:ignore[no-untyped-call]
