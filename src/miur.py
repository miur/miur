import sys
from contextlib import ExitStack

from . import curses_ext as CE
from . import iomgr
from .app import AppCursesUI, AppGlobals
from .util.envlevel import increment_envlevel
from .util.exchook import enable_warnings, log_excepthook
from .util.logger import log
from .util.pidfile import pidfile_path, send_pidfile_signal, temp_pidfile


def miur_main(g: AppGlobals) -> None:
    if g.opts.PROFILE_STARTUP:
        # FIXME: accumulate early logs into buffer, and emit them at once only after FD redir
        # OPT: print early logs immediately to STDERR -- to troubleshoot early startup
        # ALT/ADD: emit sys.audit() event to use !lttng for uniform startup profiling
        log.kpi("main")

    with ExitStack() as stack:  # MOVE:> with Application() as app:
        do = stack.enter_context
        do(enable_warnings())
        # BET:CHG: inof preventing nesting -- simply connect nested miur to the same outer session of the server
        #   NICE: propagate all the changes back to the top-level miur session in miur-vim-miur sandwich
        do(increment_envlevel("MIUR_LEVEL"))
        # MAYBE: only enable PIDFILE when run by miur_frontend() to avoid global VAR ?
        pid = do(temp_pidfile(pidfile_path()))
        # BAD: log is too early to be redirected by stdlog_redir()
        log.verbose(f"{pid=}")
        do(log_excepthook())

        # MOVE? as early as possible
        do(iomgr.stdlog_redir(g))
        g.stdscr = do(CE.curses_stdscr())

        # raise RuntimeError()

        from .curses_cmds import handle_input, resize
        from .ui.entries import FSEntry
        from .ui.root import RootWidget

        ui = AppCursesUI()
        ui.resize = lambda: resize(g)
        ui.handle_input = lambda: handle_input(g)
        g.curses_ui = ui
        xpath = getattr(g.opts, "xpath", None) or "/d/airy"
        g.root_wdg = RootWidget(FSEntry(xpath))
        # g.root_wdg.set_entity(FSEntry("/etc/udev"))

        if g.opts.bare:  # NOTE: much faster startup w/o asyncio machinery
            from .curses_cmds import g_input_handlers
            from .loop_selectors import mainloop_selectors

            def _shell_out(g: AppGlobals) -> None:
                CE.shell_out(g.stdscr)

            g_input_handlers["s"] = _shell_out
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
    if g.opts.devinstall:
        from .util import devenv

        devenv.install_venv_deps(devroot=g.opts.devroot, dev=True)
        # MAYBE: allow "continue running" after installing deps
        #   ~~ somewhat annoying in practice, probably only useful for daemon server
        sys.exit()

    if sig := g.opts.signal:
        ret = send_pidfile_signal(pidfile_path(), sig)
        sys.exit(ret if ret is None or isinstance(ret, int) else str(ret))

    # CASE: launch console on "True", quit remotely on "False"
    if (v := g.opts.ipyconsole) is not None:
        log.kpi(f"ipyconsole = {v}")
        import asyncio

        from .util.jupyter import ipyconsole_async

        sys.exit(asyncio.run(ipyconsole_async(shutdown=v)))

    # log.info(f"cwd={opts.cwd}")
    return miur_main(g)


def _live() -> None:
    # import _curses as C
    from .app import g_app as g

    stdscr = g.stdscr

    # ALT: $ jupyter console --existing miur-ipython.json
    # pylint:disable=used-before-assignment
    stdscr.addstr(4, 1, "hello")
    stdscr.refresh()
