import asyncio
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

from ipykernel.kernelapp import IPKernelApp, ioloop


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


def selectors_loop(stdscr: C.window) -> None:
    with route_signals_to_fd() as sigfd, selectors.DefaultSelector() as sel:
        sel.register(sys.stdin.fileno(), selectors.EVENT_READ, data=None)
        sel.register(sigfd, selectors.EVENT_READ, data=None)
        stdscr.nodelay(True)
        try:
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


async def amain(stdscr: C.window) -> None:
    loop = asyncio.get_running_loop()
    curses_stdin_fd = 0
    # FAIL: RuntimeError: Event loop stopped before Future completed.
    ev_shutdown = asyncio.Event()
    loop.add_signal_handler(signal.SIGINT, ev_shutdown.set)
    loop.add_signal_handler(signal.SIGWINCH, stdscr.refresh)
    stdscr.nodelay(True)
    loop.add_reader(fd=curses_stdin_fd, callback=lambda: handle_input(stdscr))
    try:
        await ev_shutdown.wait()
        # while True:
        #     await asyncio.sleep(1)
    finally:
        loop.remove_reader(fd=curses_stdin_fd)
        stdscr.nodelay(False)
        loop.remove_signal_handler(signal.SIGWINCH)
        loop.remove_signal_handler(signal.SIGINT)


def mainloop(stdscr: C.window) -> None:
    # [_] FIXME: restore in "finally" to prevent logging to curses after it exits
    log.config(write=lambda text: CE.print_curses_altscreen(stdscr, text))

    if not C.has_extended_color_support():
        raise NotImplementedError

    # MAYBE: if not opts.ipykernel: selectors_loop(stdscr)
    # asyncio.run(amain(stdscr))

    kernel = IPKernelApp.instance()
    kernel.initialize(["python", "--IPKernelApp.parent_handle=1"])
    kernel.reset_io()
    # DEBUG: raise RuntimeError()


    def setup_handler():
        loop = asyncio.get_running_loop()
        # WARN: should "loop.stop" be replaced by some kernel.stop() ?
        loop.add_signal_handler(signal.SIGINT, loop.stop)

    ioloop.IOLoop.current().add_callback(setup_handler)
    kernel.start()


def miur_none() -> None:
    with ExitStack() as stack:  # MOVE:> with Application() as app:
        stack.enter_context(increment_envlevel("MIUR_LEVEL"))
        # MAYBE: only enable PIDFILE when run by miur_opts() to avoid global VAR ?
        stack.enter_context(temp_pidfile(PIDFILE))
        stack.enter_context(log_excepthook())
        return C.wrapper(mainloop)


if TYPE_CHECKING:
    from argparse import Namespace


# TBD: frontend to various ways to run miur API with different UI
def miur_opts(opts: "Namespace") -> None:
    if sig := opts.signal:
        sys.exit(send_pidfile_signal(PIDFILE, sig))  # type:ignore

    log.info(f"cwd={opts.cwd}")
    return miur_none()


def _live() -> None:
    pass
