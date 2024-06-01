import os
import selectors
import signal
import stat
import sys
from contextlib import ExitStack, contextmanager
from typing import TYPE_CHECKING, Any, Iterator, cast

import _curses as C

from . import curses_ext as CE
from .app import AppGlobals
from .curses_cmds import handle_input
from .util.envlevel import increment_envlevel
from .util.exchook import exception_handler, log_excepthook
from .util.logger import log
from .util.pidfile import pidfile_path, send_pidfile_signal, temp_pidfile
from .util.sighandler import route_signals_to_fd


def handle_SIGWINCH(
    sel: selectors.DefaultSelector, sigfd: int, stdscr: C.window
) -> None:
    # &next BET:RFC:
    #   * give up on sigfd -- it's unreliable, as it requires some sigaction() anyway
    #   * set flag in handler -- COS we need refresh only once for all signals
    #   * propagate signal from handler to Epoll -- for timely .refresh
    who = sel.__class__.__name__
    si = int.from_bytes(os.read(sigfd, 1))
    snm = signal.Signals(si).name
    sdesc = signal.strsignal(si)
    log.warning(f"{who} {snm}={si}: {sdesc}")

    ## HACK:SRC: https://stackoverflow.com/questions/1022957/getting-terminal-width-in-c
    ##   >> make curses to calc sizes by itself (as it does on each .refresh)
    ## INFO: actually, "log.*" already does the same, but its IMPL is subject to change
    # with CE.curses_altscreen(stdscr):
    #     pass
    # [_] CHECK: do we even need full .def_prog_mode()/.endwin() here ?
    stdscr.refresh()

    ## FAIL: get KEY_RESIZE immediately, don't make Epoll wait until next keypress
    # ch = stdscr.getch()
    # assert ch == C.KEY_RESIZE, ch


def mainloop_selectors(g: AppGlobals) -> None:
    with route_signals_to_fd() as sigfd, selectors.DefaultSelector() as sel:
        sel.register(CE.CURSES_STDIN_FD, selectors.EVENT_READ, data=None)
        sel.register(sigfd, selectors.EVENT_READ, data=None)
        try:
            log.kpi("serving")
            if __debug__ and g.opts.PROFILE_STARTUP:
                return
            while True:
                for key, events in sel.select():
                    if key.fd == sigfd:
                        assert events == selectors.EVENT_READ
                        handle_SIGWINCH(sel, sigfd, g.stdscr)
                    elif key.fd == CE.CURSES_STDIN_FD:
                        assert events == selectors.EVENT_READ
                        handle_input(g)
                    else:
                        log.error(str((key, events)))
        except KeyboardInterrupt:
            pass


async def mainloop_asyncio(g: AppGlobals) -> None:
    g.stdscr.refresh()  # CHG:> app.refresh

    import asyncio

    loop = asyncio.get_running_loop()
    # FAIL: RuntimeError: Event loop stopped before Future completed.
    ev_shutdown = asyncio.Event()
    loop.add_signal_handler(signal.SIGINT, ev_shutdown.set)
    # FIND:MAYBE: don't process in handler directly, and only schedule callback ?
    loop.add_signal_handler(signal.SIGWINCH, g.stdscr.refresh)  # CHG:> app.refresh
    loop.add_reader(CE.CURSES_STDIN_FD, handle_input, g)
    try:
        log.kpi("serving")
        if __debug__ and g.opts.PROFILE_STARTUP:
            loop.call_soon(ev_shutdown.set)
        await ev_shutdown.wait()
        # while True:
        #     await asyncio.sleep(1)
    finally:
        loop.remove_reader(fd=CE.CURSES_STDIN_FD)
        loop.remove_signal_handler(signal.SIGWINCH)
        loop.remove_signal_handler(signal.SIGINT)


if TYPE_CHECKING:
    from asyncio import AbstractEventLoop


@contextmanager
def my_asyncio_loop(debug: bool = True) -> Iterator["AbstractEventLoop"]:
    import asyncio

    def _custom_exception_handler(
        loop: asyncio.AbstractEventLoop,
        context: dict[str, Any],
    ) -> None:
        loop.default_exception_handler(context)
        exc = cast(Exception, context.get("exception"))
        exception_handler(type(exc), exc, exc.__traceback__)
        # print(context)
        for t in asyncio.all_tasks():
            t.cancel()
        # loop.stop()

    ## HACK:TRY: combine my default selectors loop with asyncio
    ##   FAIL: asyncio will govern .select() inof @me
    # class MyPolicy(asyncio.DefaultEventLoopPolicy):
    #     def new_event_loop(self):
    #         selector = selectors.DefaultSelector()
    #         return asyncio.SelectorEventLoop(selector)
    # asyncio.set_event_loop_policy(MyPolicy())

    # FIXED:ERR: DeprecationWarning: There is no current event loop
    #   /usr/lib/python3.12/site-packages/tornado/ioloop.py:274:
    myloop = asyncio.new_event_loop()

    myloop.set_debug(debug)
    ## BUG: overwrites Jupyter defaults and extends Application GC lifetime
    ## FAIL: is not triggered if taskref was stored to variable [outside the loop]
    myloop.set_exception_handler(_custom_exception_handler)
    asyncio.set_event_loop(myloop)
    try:
        yield myloop
    finally:
        asyncio.set_event_loop(None)


@contextmanager
def enable_warnings(error: bool = True) -> Iterator[None]:
    if sys.warnoptions:
        return

    import warnings

    # DEBUG: ResourceWarning(asyncio), DeprecationWarning(ipython), etc.
    if not error:
        warnings.simplefilter("always")  # OR="default" to print 1st only
        return

    # SRC: https://stackoverflow.com/questions/22373927/get-traceback-of-warnings
    # def warn_with_traceback(message, category, filename, lineno, file=None, line=None):
    #     log = file if hasattr(file,'write') else sys.stderr
    #     traceback.print_stack(file=log)
    #     log.write(warnings.formatwarning(message, category, filename, lineno, line))
    # warnings.showwarning = warn_with_traceback

    warnings.filterwarnings("error")  # Treat warnings as errors
    try:
        yield
    # except Warning:
    #     log.warning(traceback.format_exc())  # print traceback
    finally:
        warnings.resetwarnings()  # Back to default behavior


def miur_main(g: AppGlobals | None = None) -> None:
    # bare: bool = True, ipykernel: bool = False
    if g is None:
        from .app import g_app
        g = g_app

    g._main = sys.modules[__name__]  # pylint:disable=protected-access

    if g.opts.PROFILE_STARTUP:
        log.kpi("main")

    with ExitStack() as stack:  # MOVE:> with Application() as app:
        do = stack.enter_context
        do(enable_warnings())
        do(increment_envlevel("MIUR_LEVEL"))
        # MAYBE: only enable PIDFILE when run by miur_frontend() to avoid global VAR ?
        do(temp_pidfile(pidfile_path()))
        do(log_excepthook())

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
        for ttyio in (sys.stdout, sys.stderr):
            if ttyio.isatty() or os.fstat(ttyio.fileno()).st_mode & stat.S_IFIFO:
                do(CE.stdio_to_altscreen(g.stdscr, ttyio))

        if g.opts.bare:  # NOTE: much faster startup w/o asyncio machinery
            from .curses_cmds import g_input_handlers

            def _shell_out(g: AppGlobals) -> None:
                CE.shell_out(g.stdscr)

            g_input_handlers["S"] = _shell_out
            return mainloop_selectors(g)

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
    from .app import g

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
    # pylint:disable=used-before-assignment
    stdscr: "C.window"
    stdscr.addstr(1, 1, "hello")
    stdscr.refresh()
