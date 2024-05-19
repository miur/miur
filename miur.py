import curses as C
import os
import selectors
import signal
import sys
from contextlib import ExitStack
from typing import TYPE_CHECKING, Final

from . import curses_ext as CE
from .curses_cmds import input_handlers
from .util.envlevel import increment_envlevel
from .util.exchook import log_excepthook
from .util.logger import log
from .util.pidfile import send_pidfile_signal, temp_pidfile
from .util.sighandler import route_signals_to_fd

# OR:(/tmp)=f"/run/user/{os.getlogin()}" os.environ.get('', "/tmp")
PIDFILE: Final[str] = os.environ.get("XDG_RUNTIME_DIR", "/tmp") + "/miur.pid"

# USAGE: time mi --backend=asyncio
PROFILE_STARTUP = True  # =DEBUG


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


def handle_input(stdscr: C.window) -> None:
    wch = stdscr.get_wch()
    cmd = input_handlers.get(wch, None)
    comment = f" ({cmd.__name__})" if cmd else ""
    log.warning(repr(wch) + comment)
    if cmd:
        # WARN: last stmt in loop COS: may raise SystemExit
        cmd(stdscr)


def mainloop_selectors(stdscr: C.window) -> None:
    with route_signals_to_fd() as sigfd, selectors.DefaultSelector() as sel:
        sel.register(sys.stdin.fileno(), selectors.EVENT_READ, data=None)
        sel.register(sigfd, selectors.EVENT_READ, data=None)
        stdscr.nodelay(True)
        try:
            while True:
                if globals().get("PROFILE_STARTUP"):
                    log.kpi("serving")
                    break
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
        if globals().get("PROFILE_STARTUP"):
            loop.call_soon(ev_shutdown.set)
            log.kpi("serving")
        await ev_shutdown.wait()
        # while True:
        #     await asyncio.sleep(1)
    finally:
        loop.remove_reader(fd=curses_stdin_fd)
        stdscr.nodelay(False)
        loop.remove_signal_handler(signal.SIGWINCH)
        loop.remove_signal_handler(signal.SIGINT)


def mainloop_ipython(stdscr: C.window) -> None:
    import ipykernel.kernelapp as IK

    if not sys.warnoptions:
        __import__("warnings").simplefilter("default")  # DEBUG: ResourceWarning

    os.environ["PYDEVD_DISABLE_FILE_VALIDATION"] = "1"  # =debugpy
    kernel = IK.IPKernelApp.instance()
    kernel.connection_file = "miur-ipython.json"
    kernel.parent_handle = 1  # EXPL: suppress banner with conn details
    kernel.outstream_class = None  # type:ignore

    # ERR: DeprecationWarning: There is no current event loop
    #   /usr/lib/python3.12/site-packages/tornado/ioloop.py:274:
    kernel.initialize([])  # type:ignore  # ["python", "--debug"]

    # ALT:(outstream_class):FIXED: restore redirected stdout/sdterr for !pdb/etc
    # kernel.reset_io()  # type:ignore

    # [_] CHECK
    ns = kernel.shell.user_ns  # type:ignore
    ns["kernel"] = kernel
    ns["stdscr"] = stdscr
    # ns['_miur'] = __import__("__main__")  # <TODO

    # raise RuntimeError()

    async def run_mainapp(stdscr: C.window) -> None:
        await mainloop_asyncio(stdscr)
        kernel.io_loop.stop()
        ## OR: together
        # async with asyncio.TaskGroup() as tg:
        #     _t1 = tg.create_task(mainloop_asyncio(stdscr))
        #     _t2 = tg.create_task(ipykernel(...))

    tasks = []

    ## FIXED: restore ignored SIGINT
    # BAD: we can't raise exceptions from inside native sig handlers
    #   signal.signal(signal.SIGINT, lambda si,fr: None)
    # FAIL: no running event loop
    #   loop = asyncio.get_running_loop()
    # def setup_mainapp() -> None:
    #     loop = asyncio.get_running_loop()
    #     # kio = kernel.io_loop
    #     # loop.add_signal_handler(signal.SIGINT, lambda: kio.add_callback(kio.stop))
    #     tasks.append(loop.create_task(run_mainapp(stdscr), name='main_app'))

    kio = IK.ioloop.IOLoop.current()  # type:ignore
    kio.asyncio_loop.set_debug(True)  # DEBUG
    # kio.add_callback(setup_mainapp)
    tasks.append(kio.asyncio_loop.create_task(run_mainapp(stdscr), name="main_app"))

    # HACK: patch to have more control over messy IPython loop
    #   >> inof "kio.asyncio_loop.run_forever()"
    kio.asyncio_loop.start = lambda: None
    kernel.start()  # type:ignore
    # ALT:BAD: try: kernel.start(); finally: kio.close()
    __import__("asyncio").run(run_mainapp(stdscr))


def miur_none(backend: str | None = None) -> None:
    if backend is None:
        backend = "selectors"
    if backend == "selectors":
        mainloop = mainloop_selectors
    elif backend == "asyncio":
        # NOTE: asyncio.iscoroutinefunction(someFunc)
        # pylint:disable=unnecessary-lambda-assignment
        mainloop = lambda stdscr: __import__("asyncio").run(mainloop_asyncio(stdscr))
    elif backend == "ipython":
        mainloop = mainloop_ipython
    else:
        raise NotImplementedError

    with ExitStack() as stack:  # MOVE:> with Application() as app:
        stack.enter_context(increment_envlevel("MIUR_LEVEL"))
        # MAYBE: only enable PIDFILE when run by miur_opts() to avoid global VAR ?
        stack.enter_context(temp_pidfile(PIDFILE))
        stack.enter_context(log_excepthook())
        stdscr = stack.enter_context(CE.curses_stdscr())
        stack.enter_context(CE.stdio_to_altscreen(stdscr))  # OR: log_to_altscreen()
        return mainloop(stdscr)


if TYPE_CHECKING:
    from argparse import Namespace


# TBD: frontend to various ways to run miur API with different UI
def miur_opts(opts: "Namespace") -> None:
    if sig := opts.signal:
        sys.exit(send_pidfile_signal(PIDFILE, sig))  # type:ignore

    ## TODO: embedded console/client
    # $ jupyter console --existing miur-ipython.json

    log.info(f"cwd={opts.cwd}")
    return miur_none(backend=opts.backend)


def _live() -> None:
    # pylint:disable=used-before-assignment
    stdscr: "C.window"
    stdscr.addstr(1, 1, "hello")
    stdscr.refresh()
