import sys
from contextlib import ExitStack

from . import curses_ext as CE
from .app import AppGlobals
from .util.envlevel import increment_envlevel
from .util.exchook import enable_warnings, log_excepthook
from .util.logger import log
from .util.pidfile import pidfile_path, send_pidfile_signal, temp_pidfile


def miur_main(g: AppGlobals | None = None) -> None:
    # bare: bool = True, ipykernel: bool = False
    if g is None:
        from .app import g_app

        g = g_app  # FIXED: !mypy idiosyncrasies

    g._main = sys.modules[__name__]  # pylint:disable=protected-access

    if g.opts.PROFILE_STARTUP:
        # FIXME: accumulate early logs into buffer, and emit them at once only after FD redir
        # OPT: print early logs immediately to STDERR -- to troubleshoot early startup
        # ALT/ADD: emit sys.audit() event to use !lttng for uniform startup profiling
        log.kpi("main")

    with ExitStack() as stack:  # MOVE:> with Application() as app:
        do = stack.enter_context
        do(enable_warnings())
        do(increment_envlevel("MIUR_LEVEL"))
        # MAYBE: only enable PIDFILE when run by miur_frontend() to avoid global VAR ?
        do(temp_pidfile(pidfile_path()))
        do(log_excepthook())

        # g.io.pipein =
        # g.io.pipeout =
        # g.io.ttyin =
        # g.io.ttyout =
        # g.io.mixedout =
        # g.io.logsout =

        # NOTE: this redirection is needed not unconditionally, but only if we use curses
        ## TEMP:DISABLED: due to kernel.outstream_class "echo"
        ##   NEED: use separate explicit FD for explicit PIPE -- to prevent their redir by Jupyter
        for nm in "stdin stdout stderr".split():
            if not getattr(sys, nm).isatty():
                # TBD: open os.pipe to cvt libc.stderr into py.log inof spitting over TTY
                do(CE.redir_stdio_nm(nm))
                if nm == "stdout":
                    # NOTE: refresh closed FD
                    #   TBD: restore back on scope
                    log.write = sys.stdout.write

        g.stdscr = do(CE.curses_stdscr())

        # [_] FIXME: add hooks individually
        # BAD: { mi | cat } won't enable altscreen, and !cat will spit all over TTY
        #   ALSO: even if it's not !cat, pipeline still may eventually print something to TTY
        #   WKRND: always use altscreen unless redir to file/socket (until PERF measurements)
        #     BAD it won't help if other app produces output at different timings
        ## TEMP:DISABLED: due to kernel.outstream_class "echo"
        ##   TRY: wrap only "echo" __std*__.write
        import os
        import stat

        for ttyio in (sys.stdout, sys.stderr):
            if ttyio.isatty() or os.fstat(ttyio.fileno()).st_mode & stat.S_IFIFO:
                do(CE.stdio_to_altscreen(g.stdscr, ttyio))

        if g.opts.bare:  # NOTE: much faster startup w/o asyncio machinery
            from .curses_cmds import g_input_handlers
            from .loop_selectors import mainloop_selectors

            def _shell_out(g: AppGlobals) -> None:
                CE.shell_out(g.stdscr)

            g_input_handlers["S"] = _shell_out
            return mainloop_selectors(g)

        from .loop_asyncio import mainloop_asyncio, my_asyncio_loop

        myloop = do(my_asyncio_loop())

        if g.opts.ipykernel:
            from .util.jupyter import inject_ipykernel_into_asyncio

            # pylint:disable=protected-access
            myns = {"g": g, "stdscr": g.stdscr, "_main": g._main}
            inject_ipykernel_into_asyncio(myloop, myns)

        import asyncio

        return asyncio.run(mainloop_asyncio(g), loop_factory=lambda: myloop)


# TBD: frontend to various ways to run miur API with different UI
def miur_frontend(g: AppGlobals) -> None:
    c = g.opts.color  # NB: we always have .default set
    if c is None:
        # FIXME: check actual fd *after* all redirection OPT
        # BET: don't reassing cmdline opts -- treat them as Final, and SEP from "state"
        c = sys.stdout.isatty()
    log.config(termcolor=c)

    if g.opts.PROFILE_STARTUP:
        log.kpi("argparse")

    if sig := g.opts.signal:
        ret = send_pidfile_signal(pidfile_path(), sig)
        sys.exit(ret if ret is None or isinstance(ret, int) else str(ret))

    if g.opts.ipyconsole:
        import asyncio

        from .util.jupyter import ipyconsole_async

        # ALT: $ jupyter console --existing miur-ipython.json
        # > stdscr.addstr(1, 1, "hello")
        # > stdscr.refresh()
        asyncio.run(ipyconsole_async())
        sys.exit()

    # log.info(f"cwd={opts.cwd}")
    return miur_main(g)


def _live() -> None:
    import _curses as C

    # pylint:disable=used-before-assignment
    stdscr: "C.window"
    stdscr.addstr(1, 1, "hello")
    stdscr.refresh()
