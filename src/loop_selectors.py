import os
import selectors
import signal

import _curses as C

from . import iomgr
from .app import AppGlobals
from .curses_cmds import handle_input
from .util.logger import log
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
    def _doexit() -> None:
        raise SystemExit()

    g.doexit = _doexit

    with route_signals_to_fd() as sigfd, selectors.DefaultSelector() as sel:
        sel.register(iomgr.CURSES_STDIN_FD, selectors.EVENT_READ, data=None)
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
                    elif key.fd == iomgr.CURSES_STDIN_FD:
                        assert events == selectors.EVENT_READ
                        handle_input(g)
                    else:
                        log.error(str((key, events)))
        except KeyboardInterrupt:
            pass
