#!/usr/bin/env -S python -SIB -X faulthandler
#!/usr/bin/python -SIB
#!/usr/bin/env -S python -SIB -Ximporttime
#!/usr/bin/env -S python -SIB -m cProfile -s cumulative --
## BET? make a "dev-launcher/ctlpanel" for !miur with possible options mapped to flags
##   >> put it out of "src" folder and optionally distribute it as a separate pkgs
##   ALSO? split "miur_log" (and some others) to top-level multipackage
##     (=make [m] in !miur stand for [m]odular)
# ALT(-B): -O -X pycache_prefix=~/.cache/miur
#   -I(-P/s/E)
# PERF:DEBUG: $ PYTHONPATH=/d/just python -PsSB [-X importtime | -X tracemalloc] -m miur
#   OR: $ python -m cProfile -s cumulative -- =miur
#   [_] TODO: run all of them through flags
#     e.g. set flags=noninteractive (to exit fast) and then exec(cmdline)
#   [_] ALSO: directly run Jupyther kernel and console by sourcing from inside my code
#     /d/coastline/fleur/cfg/gdb-jupyter.py
# %SUMMARY: frontend
# %USAGE: $ mi || miur || . =mi && m
""":"
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


## CASE:(link): realdir [/data/miur] vs symlink [/d/miur] vs rootlink [/m] vs usrbin
## TODO: write :/t/ pytest checking all possible ways to launch !miur
#   /m -> /usr/bin/miur ==> /usr/lib/python3.13/site-packages/miur/__main__.py
#   /usr/local/bin/m -> miur -> /d/miur/src/__main__.py -> /data/aura/miur/src/__main__.py
#   /home/user/.local/bin/m -> /d/miur/.venv/lib/python3.13/site-packages/miur/__main__.py
#   ./m -> /data/aura/miur/src/./__main__.py  (PWD=/d/miur)
#      ~ /data/aura/miur/./src/__main__.py
#   pip install -e . -> ~/.local/lib/python3.13/site-packages/miur/ -> /d/miur/src
def guess_devroot() -> tuple[str, str]:
    ## HACK:PERF:(-3ms): don't import os.path on POSIX during DEV startup
    ##   BAD: importing "curses" imports "os" anyways
    # if sys.path and sys.path[0][0] == "/":
    #   return "."

    # OR: fs = __import__("os.path", fromlist=[""])
    import os
    import os.path as fs

    ## WARN: busybox-like name-aliases mean you should *always* realpath(__file__) at least for "nm"
    ## WARN: should strip either all levels of symlinks or none, as intermediates can be located anywhere
    #    if fs.islink(path): return fs.join(fs.dirname(path), os.readlink(path))
    # OR? srcdir = fs.dirname(fs.realpath(sys.modules["__main__"].__file__))
    srcdir = fs.dirname(fs.realpath(__file__))
    if srcdir == "/" or (p := fs.dirname(srcdir)) == "/":
        raise RuntimeError(f"Err: refusing to treat '/' as 'site' for {__file__}")
        # ALT: from ._pkg import __appname__
        # return "miur", "miur"

    ## TODO: auto-simplify path to symlinked variant
    # pwd = os.environ.get("PWD", "")
    # if pwd and not p.startswith(pwd + "/"):  # and fs.samefile(p, pwd):
    #     p = p.replace(fs.realpath(pwd), pwd)  # .rsplit('/')[0]

    return p, fs.basename(srcdir)


# BAD: can't move to :/src/pkgenv/devroot.py COS relative import won't work w/o this cls
class ensure_devsrc_has_package:
    def __init__(self) -> None:
        self._orig: str
        self._nm: str
        self._devroot: str

    def __enter__(self) -> str:
        global __package__
        # ALSO?(allow recursive entry): if ==self._nm: return
        if __package__ is not None:
            return ""
        # PERF:(dont asgn in ctor): spend time on heuristics only when needed
        self._devroot, self._nm = guess_devroot()
        sys.path.insert(0, self._devroot)
        self._orig = __package__
        __package__ = self._nm
        return self._devroot

    def __exit__(self, _et, _exc, _tb):  # type:ignore[no-untyped-def]
        global __package__
        if not hasattr(self, "_orig"):
            # assert __package__ == self._orig, "Err: smb changed your {__package__=} smwr"
            return
        assert __package__ == self._nm, "Err: smb changed your {__package__=} smwr"
        __package__ = self._orig
        assert sys.path[0] == self._devroot
        sys.path.remove(self._devroot)


# RENAME? main_entrypoint()
def miur_autoselect(argv: list[str] = sys.argv) -> None:
    # CASE:(envctl): hardened [app] vs flexible [lib]
    if __name__ == "__main__":
        sys.path = [p for p in sys.path if not p.endswith(".zip")]
        # sys.dont_write_bytecode = '-c' in argv or '--clean' in argv

    # CASE:(install): devsrc [gitclone or tarball] vs sitepkg [system or xdguser] vs apt/pacman [/usr/bin]
    with ensure_devsrc_has_package() as devroot:

        # CASE: selectors [bare] vs asyncio [argparse]
        #   TRY:IDEA: "perceivable startup speed" : draw asap, do evels later
        #     - draw current screen by shortest import path
        #        == so when user will be gathering his bearings, we could load evels in bkgr
        #     - start asyncio only *after* drawing first frame
        #     - migrate further drawing to asyncio
        # HACK:(--bare): use alt-name "mi-" to run a faster (but limited) raw EPOLL backend
        # bare = argv[0].rpartition("/")[2] in ("mi-", "miur-")

        # CASE:(pkgdeps): preinst [systemwide or extvenv] vs autovenv [devsrc:rw vs xdgcache]
        #   WARN: /usr/src/<devsrc> is readonly -> can't create .venv in <devsrc>
        #   PERF: run --bare w/o .venv; NEED: install all deps by system pkg manager
        if devroot:  # and not bare:
            if (vp := sys.prefix) == (bp := sys.base_prefix):
                from .util.devenv import ensure_venv

                # BAD: shell_out() will be inside that .venv too
                #   >> ATT: you shouldn't run *any* python programs in that nested shell
                # MAYBE: make a frontend to miur (like "fleur/ctl" did)
                #   >> move all dev-helpers there and access miur only through it
                #   &why keep only essential features in primary codebase
                # CASE:(dev): gitclone as user (reqs.txt) vs gitclone as developer (+reqs_dev.txt)
                ensure_venv(devroot, dev=True)

            from .util.logger import log

            log.sep()
            # BAD: log is too early to be redirected by stdlog_redir()
            #   IDEA: by default -- accum early logs in list/ring/stringio,
            #     until .write is set for the first time, and then dump all of them at once
            venv_path = "---" if vp == bp else vp if bp == "/usr" else f"{vp} ; {bp}"
            log.state(f"(.venv): {venv_path}")

            from .util.devenv import get_py_args

            cmdline = " ".join(repr(a) if " " in a else a for a in get_py_args())
            log.state(f"<$ {cmdline}")
            log.kpi("entrypoint")  # if o.PROFILE_STARTUP:

        ## FIXME: restore for !jupyter OR remove
        # from .app import g_app
        # # ALT?(sys.modules[__main__]): will it work in !jupyter ?
        # g_app._main = sys.modules[__name__]  # pylint:disable=protected-access
        # o = g_app.opts
        # o.bare = bare
        # o.devroot = devroot

        # CASE: bare vs argparse
        #   PERF: faster startup w/o importing ArgumentParser (128ms vs 115ms)
        #     (and even faster w/o processing all cmdline args)
        if len(argv) == 1 or (len(argv) > 1 and argv[1] == "--"):
            # argv = sys.argv[1:sys.argv.index('--')] if '--' in sys.argv else sys.argv[1:]

            from .app import g_app
            from .miur import miur_main

            def entrypoint() -> None:
                return miur_main(g_app)

        else:
            # ALT: main = __import__("importlib").import_module(".cli", package="miur").main
            from .cli import miur_argparse

            def entrypoint() -> None:
                return miur_argparse(argv)

    return entrypoint()


# CHECK: will this guard work with mp=spawn ?
if __name__ == "__main__":
    # FIXME: should be patched during installation depending on usage/location inof dynamic selection
    #   TODO: during PKGBUILD replace whole "__miur__.py" file with "miur_argparse"
    #   ALT: if we symlink same sources into multiple locations -- dynamic selection still may be useful
    sys.exit(miur_autoselect())  # type:ignore[func-returns-value]
