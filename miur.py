import curses as C
import os
import selectors
import signal
import sys
from contextlib import ExitStack, contextmanager
from typing import TYPE_CHECKING, Any, Final, Iterator

from . import curses_ext as CE
from .curses_cmds import input_handlers
from .util.exc import log_excepthook
from .util.log import log
from .util.sig import route_signals_to_fd

if TYPE_CHECKING:
    from argparse import Namespace

# CHG: XDG_RUNTIME_DIR=/run/user/1000 + /miur/pid
PIDFILE: Final = "/t/miur.pid"


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
                        with CE.curses_altscreen(stdscr):
                            pass

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


@contextmanager
def temp_pidfile(pidfile: str) -> Iterator[None]:
    with open(pidfile, "w", encoding="utf-8") as f:
        f.write(str(os.getpid()))
    try:
        yield
    finally:
        os.remove(pidfile)


@contextmanager
def miur_envlevel(varname: str) -> Iterator[int]:
    lvl = int(os.environ.get(varname, "0"))
    if lvl != 0:
        log.error(f"avoid nesting {lvl=}")
        sys.exit(1)
    os.environ[varname] = str(lvl + 1)
    try:
        yield lvl
    finally:
        os.environ[varname] = str(lvl)


def miur_none() -> None:
    # with Application() as app:
    with miur_envlevel("MIUR_LEVEL"), temp_pidfile(PIDFILE), log_excepthook():
        return C.wrapper(mainloop)


# TBD: frontend to various ways to run miur API with different UI
def miur_opts(opts: "Namespace") -> None:
    if sig := opts.signal:
        # MAYBE: check by short LOCK_EX if any LOCK_SH present (i.e. main miur running)
        try:
            # SEIZE: trbs/pid: Pidfile featuring stale detection and file-locking ⌇⡦⠿⣢⢔
            #   https://github.com/trbs/pid
            with open(PIDFILE, "r", encoding="utf-8") as f:
                pid = int(f.read())
        except FileNotFoundError as exc:
            log.error(f"fail {PIDFILE=} | {exc}")
            sys.exit(1)

        log.warning(f"sending signal={sig} to {pid=}")
        try:
            os.kill(pid, sig)
        except ProcessLookupError as exc:
            log.error(f"fail {pid=} | {exc}")
            sys.exit(1)
        sys.exit(0)

    log.info(f"cwd={opts.cwd}")
    return miur_none()


def _live() -> None:
    pass
