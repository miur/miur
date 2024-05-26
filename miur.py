import curses as C
import os
import selectors
import signal
import sys
from contextlib import ExitStack, contextmanager
from typing import TYPE_CHECKING, Any, Iterator, cast

from . import _app
from . import curses_ext as CE
from .curses_cmds import handle_input
from .util.envlevel import increment_envlevel
from .util.exchook import log_excepthook, exception_handler
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


def mainloop_selectors(stdscr: C.window) -> None:
    with route_signals_to_fd() as sigfd, selectors.DefaultSelector() as sel:
        sel.register(sys.stdin.fileno(), selectors.EVENT_READ, data=None)
        sel.register(sigfd, selectors.EVENT_READ, data=None)
        stdscr.nodelay(True)
        try:
            log.kpi("serving")
            if __debug__ and _app.PROFILE_STARTUP:
                return
            while True:
                for key, events in sel.select():
                    if key.fd == sigfd:
                        assert events == selectors.EVENT_READ
                        handle_SIGWINCH(sel, sigfd, stdscr)
                    elif key.fd == sys.stdin.fileno():
                        assert events == selectors.EVENT_READ
                        handle_input(stdscr)
                    else:
                        log.error((key, events))
        except KeyboardInterrupt:
            pass
        finally:
            stdscr.nodelay(False)


async def mainloop_asyncio(stdscr: C.window) -> None:
    stdscr.refresh()  # CHG:> app.refresh

    import asyncio

    loop = asyncio.get_running_loop()
    curses_stdin_fd = 0
    # FAIL: RuntimeError: Event loop stopped before Future completed.
    ev_shutdown = asyncio.Event()
    loop.add_signal_handler(signal.SIGINT, ev_shutdown.set)
    # FIND:MAYBE: don't process in handler directly, and only schedule callback ?
    loop.add_signal_handler(signal.SIGWINCH, stdscr.refresh)  # CHG:> app.refresh
    stdscr.nodelay(True)
    loop.add_reader(fd=curses_stdin_fd, callback=lambda: handle_input(stdscr))
    try:
        log.kpi("serving")
        if __debug__ and _app.PROFILE_STARTUP:
            loop.call_soon(ev_shutdown.set)
        await ev_shutdown.wait()
        # while True:
        #     await asyncio.sleep(1)
    finally:
        loop.remove_reader(fd=curses_stdin_fd)
        stdscr.nodelay(False)
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
        exception_handler(type(exc), exc, None)
        # print(context)
        for t in asyncio.all_tasks():
            t.cancel()
        # loop.stop()

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


def inject_ipykernel_into_asyncio(
    myloop: "AbstractEventLoop",
    myns: dict[str, Any],
) -> None:
    if sys.flags.isolated:
        __import__("site").main()  # lazy init for "site" in isolated mode
    # FIXED:DEPR: can be removed for !jupyter_core>=6
    os.environ["JUPYTER_PLATFORM_DIRS"] = "1"
    import ipykernel.kernelapp as IK

    os.environ["PYDEVD_DISABLE_FILE_VALIDATION"] = "1"  # =debugpy
    kernel = IK.IPKernelApp.instance()
    kernel.connection_file = "miur-ipython.json"
    kernel.parent_handle = 1  # EXPL: suppress banner with conn details
    # EXPL:(outstream_class): prevent stdout/stderr redirection
    #   ALT: reset after .initialize() to see !pdb/etc. output
    #     kernel.reset_io()  # EXPL:FIXED: restore redirected stdout/sdterr
    kernel.outstream_class = None  # type:ignore

    log.warning(lambda: "bef kinit: %d" % __import__("threading").active_count())
    kernel.initialize([])  # type:ignore  # OR:([]): ["python", "--debug"]
    log.warning(lambda: "aft kinit: %d" % __import__("threading").active_count())

    ipyloop = IK.ioloop.IOLoop.current()  # type:ignore
    assert myloop == ipyloop.asyncio_loop, "Bug: IPython doesn't use my own global loop"

    # HACK: monkey-patch to have more control over messy IPython loop
    #   ALT:BAD: try: kernel.start(); finally: kio.close()
    old_start = ipyloop.start
    ipyloop.start = lambda: None  # EXPL: inof "kio.asyncio_loop.run_forever()"
    try:
        kernel.start()  # type:ignore
    finally:
        ipyloop.start = old_start

    ns = kernel.shell.user_ns  # type:ignore
    ns["ipk"] = kernel
    ns.update(myns)


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


def miur_none(backend: str | None = None) -> None:
    if _app.PROFILE_STARTUP:
        log.kpi("main")

    if backend is None:
        backend = "selectors"

    with ExitStack() as stack:  # MOVE:> with Application() as app:
        do = stack.enter_context
        do(enable_warnings())
        do(increment_envlevel("MIUR_LEVEL"))
        # MAYBE: only enable PIDFILE when run by miur_opts() to avoid global VAR ?
        do(temp_pidfile(pidfile_path()))
        do(log_excepthook())

        # TBD: redir fd=0/1 to tty for curses to disentangle from cmdline stdin/stdout
        stdscr = do(CE.curses_stdscr())

        if sys.stdout.isatty():
            # OPT:(logs): tty-altscreen | stdout-pipe-redir | custom fd | memring
            #   IDEA:(tty-altscreen): when we shell-out -- accumulate logs in memring,
            #   and then dump them into altscreen, when you return from shell-out
            do(CE.stdio_to_altscreen(stdscr))  # OR: log_to_altscreen()

        if backend == "selectors":
            return mainloop_selectors(stdscr)

        if backend in ("asyncio", "ipython"):
            myloop = do(my_asyncio_loop())

            # FIXME:CHG: only if OPT=--ipython
            #   otherwise: enable ipython on keybind='S-k'
            if backend == "ipython":
                # NOTE: make it easier to see ipython loading issues
                # FAIL: doesn't work together with stdio_to_altscreen()
                # with CE.curses_altscreen(stdscr):
                myns = {
                    "stdscr": stdscr,
                    "_miur": __import__("__main__"),
                    "mi": sys.modules[__name__],
                }
                inject_ipykernel_into_asyncio(myloop, myns)

            import asyncio

            return asyncio.run(mainloop_asyncio(stdscr), loop_factory=lambda: myloop)
        raise NotImplementedError


if TYPE_CHECKING:
    from argparse import Namespace


# TBD: frontend to various ways to run miur API with different UI
def miur_opts(opts: "Namespace") -> None:
    if opts.color is not None:
        log.config(termcolor=opts.color.value)

    if _app.PROFILE_STARTUP:
        log.kpi("argparse")

    if sig := opts.signal:
        sys.exit(send_pidfile_signal(pidfile_path(), sig))  # type:ignore

    ## TODO: embedded console/client
    # $ jupyter console --existing miur-ipython.json

    # log.info(f"cwd={opts.cwd}")
    return miur_none(backend=opts.backend)


def _live() -> None:
    # pylint:disable=used-before-assignment
    stdscr: "C.window"
    stdscr.addstr(1, 1, "hello")
    stdscr.refresh()
