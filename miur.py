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


def mainloop(stdscr: C.window) -> None:
    # [_] FIXME: restore in "finally" to prevent logging to curses after it exits
    log.config(write=lambda text: CE.print_curses_altscreen(stdscr, text))

    if not C.has_extended_color_support():
        raise NotImplementedError

    with route_signals_to_fd() as sigfd, selectors.DefaultSelector() as sel:
        sel.register(sys.stdin.fileno(), selectors.EVENT_READ, data=None)
        sel.register(sigfd, selectors.EVENT_READ, data=None)

        stdscr.nodelay(True)
        try:
            while True:
                for key, events in sel.select():

                    # &next BET:RFC:
                    #   * give up on sigfd -- it's unreliable, as it requires some sigaction() anyway
                    #   * set flag in handler -- COS we need refresh only once for all signals
                    #   * propagate signal from handler to Epoll -- for timely .refresh
                    if key.fd == sigfd:
                        assert events == selectors.EVENT_READ

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

                        continue

                    if key.fd == sys.stdin.fileno():
                        assert events == selectors.EVENT_READ
                        wch = stdscr.get_wch()
                        cmd = input_handlers.get(wch, None)
                        comment = f" ({cmd.__name__})" if cmd else ""
                        log.warning(repr(wch) + comment)
                        if cmd:
                            # WARN: last stmt in loop COS: may raise SystemExit
                            cmd(stdscr)
                        continue

                    log.error((key, events))
        except KeyboardInterrupt:
            pass
        finally:
            stdscr.nodelay(False)


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
        sys.exit(send_pidfile_signal(PIDFILE, sig))

    log.info(f"cwd={opts.cwd}")
    return miur_none()


def _live() -> None:
    pass
