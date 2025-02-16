from .app import AppCursesUI, AppGlobals
from .util.envlevel import increment_envlevel, save_choosedir
from .util.exchook import enable_warnings, log_excepthook
from .util.logger import log
from .util.pidfile import pidfile_path, send_pidfile_signal, temp_pidfile


def miur_main(g: AppGlobals) -> None:
    if g.opts.PROFILE_STARTUP:
        # FIXME: accumulate early logs into buffer, and emit them at once only after FD redir
        # OPT: print early logs immediately to STDERR -- to troubleshoot early startup
        # ALT/ADD: emit sys.audit() event to use !lttng for uniform startup profiling
        log.kpi("main")

    from contextlib import ExitStack

    with ExitStack() as stack:  # MOVE:> with Application() as app:
        do = stack.enter_context
        do(enable_warnings())
        # BET:CHG: inof preventing nesting -- simply connect nested miur to the same outer session of the server
        #   NICE: propagate all the changes back to the top-level miur session in miur-vim-miur sandwich
        do(increment_envlevel("MIUR_LEVEL"))
        # MAYBE: only enable PIDFILE when run by miur_frontend() to avoid global VAR ?
        pid = do(temp_pidfile(pidfile_path()))
        # BAD: log is too early to be redirected by stdlog_redir()
        log.state(f"{pid=}")
        do(log_excepthook())

        from . import iomgr

        # MOVE? as early as possible
        do(iomgr.stdlog_redir(g))
        log.sep()

        from .integ.any_spawn import mp_join_children

        do(mp_join_children())

        from . import curses_ext as CE

        g.stdscr = do(CE.curses_stdscr())

        # raise RuntimeError()

        from . import keymap as KM
        from .entity.base import Entity
        from .entity.fsentry import FSAuto
        from .entity.rootnode import RootNode
        from .integ.aura import keytable_insert_aura_pathes
        from .ui.root import RootWidget

        crashsafe = True
        if g.opts.ipykernel:
            crashsafe = True

        def handle_input_safe() -> None:
            """Don't exit !miur when developing in REPL or CLI"""
            try:
                KM.handle_input(g)
            except Exception as exc:  # pylint:disable=broad-exception-caught
                from .util.exchook import log_exc

                log_exc(exc)
                # FUT:RELI: rollback to prev good state (atomic transaction) ※⡧⢁⢦⢽
                #   >> stability for both devs and end-user
                # ALT:FAIL:(CE.resize()): will re-trigger exception in .redraw()
                g.stdscr.refresh()

        ui = AppCursesUI()
        ui.resize = CE.resize
        ui.handle_input = handle_input_safe if crashsafe else lambda: KM.handle_input(g)
        g.curses_ui = ui
        g.keytableroot = keytable_insert_aura_pathes(KM.g_modal_default)
        KM.modal_switch_to(None)

        # f21 [_] DEV miur --remember-url=./cwdurl vs --choosedir=./cwd
        rootnode = RootNode()
        g.root_wdg = RootWidget(rootnode)
        # g.root_wdg.set_entity(FSEntry("/etc/udev"))

        import os

        if (rhist := g.opts.remember_hist) and os.access(rhist, os.R_OK):
            with open(rhist, "r", encoding="utf-8") as f:
                if text := f.read():
                    g.root_wdg._navi._hist.load(text)

        xpath = getattr(g.opts, "xpath", None)
        ent: Entity
        if xpath is None:
            ent = FSAuto(os.getenv("PWD", "") or os.getcwd(), rootnode)
        elif xpath == "":
            ent = rootnode
        else:
            ent = FSAuto(xpath, rootnode)
        log.state(f"{xpath=} -> {ent=}")
        ## [_] FIXME: make "xpath" usable again TEMP: use rhist to restore states
        #   * should generate whole chain of entities starting from RootNode
        #     ATT: they should be cached in _vlst, and we should get the same node when _vlst is re-generated
        #   * should be applied *after* loading rhist, and simply jumped to preloaded node (if present)
        #   * can be either unix=$CWD or file:// or schema=miur:// (or miur:hist-stack://) or arbitrary web URL
        # g.root_wdg.jump_to(ent)
        # g.root_wdg.set_entity(ent)  # <RENAME? "set_rootentity(ent)" to emphasize its isolation

        # TEMP:HACK: directly append stdin to current node
        if f := g.io.pipein:
            v = g.root_wdg._navi._view
            cls = g.opts.stdinfmt or FSAuto
            lst = []
            i = 1
            # WARN: offset is in chars/codepoints inof bytes (same with .read(size=chars))
            cpoff = 0  # BAD:(no byte offset): sys.stdin.tell() not supported
            while cpoff < 4096 and (line := f.readline(4096)):
                # TODO: allow re-interpreting arbitrary words/lines as paths and vice versa
                # RND:(xpath): use "cwd" as .loci for euphemeral entries
                # FIXME: put into independent linked node, inof extending baselist
                lst.append(
                    cls(line.removesuffix("\n"), parent=v._ent)
                )  # , loci=(xpath, f":{i}")))
                i += 1
                cpoff += len(line)
            v._wdg.assign(v._xfm_lst + lst)

        rurl = g.opts.remember_url
        rcwd = g.opts.choosedir
        if rhist or rurl or rcwd:
            do(save_choosedir(rhist, rurl, rcwd))

        if g.opts.bare:  # NOTE: much faster startup w/o asyncio machinery
            from .loop_selectors import mainloop_selectors

            def _shell_out(g: AppGlobals) -> None:
                CE.shell_out(g.stdscr)

            ## FAIL: fixes shell only in _modal_defaults, but not _modal_comma
            ##   >> THINK: better way to overcome it, e.g. replace API itself
            # g.keytableroot["s"] = _shell_out
            raise NotImplementedError("FIXME: should properly replace underlying shell")

            log.state("loop=selectors")
            return mainloop_selectors(g)

        from .loop_asyncio import mainloop_asyncio, my_asyncio_loop

        myloop = do(my_asyncio_loop())

        if g.opts.ipykernel:
            from .util.jupyter import inject_ipykernel_into_asyncio

            # pylint:disable=protected-access
            myns = {"g": g, "stdscr": g.stdscr, "_main": g._main}
            inject_ipykernel_into_asyncio(myloop, myns)

        import asyncio

        log.state("loop=asyncio")
        return asyncio.run(mainloop_asyncio(g), loop_factory=lambda: myloop)


# TBD: frontend to various ways to run miur API with different UI
def miur_frontend(g: AppGlobals) -> None:
    import sys

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

    # pylint:disable=used-before-assignment
    stdscr.addstr(4, 1, "hello")
    stdscr.refresh()
