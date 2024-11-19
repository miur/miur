import os
import selectors
import signal

import _curses as C

from . import iomgr
from .app import AppGlobals
from .util.logger import log
from .util.sighandler import route_signals_to_fd


def handle_SIGWINCH(sel: selectors.DefaultSelector, sigfd: int, g: AppGlobals) -> None:
    # &next BET:RFC:
    #   * give up on sigfd -- it's unreliable, as it requires some sigaction() anyway
    #   * set flag in handler -- COS we need refresh only once for all signals
    #   * propagate signal from handler to Epoll -- for timely .refresh
    who = sel.__class__.__name__
    si = int.from_bytes(os.read(sigfd, 1))
    snm = signal.Signals(si).name
    sdesc = signal.strsignal(si)
    sz = "{}x{}".format(*g.stdscr.getmaxyx())
    log.warning(f"{who} {snm}={si}: {sdesc} [{sz}]")

    ## FAIL: get KEY_RESIZE immediately, don't make Epoll wait until next keypress
    # ch = stdscr.getch()
    # assert ch == C.KEY_RESIZE, ch
    g.curses_ui.resize()


def mainloop_selectors(g: AppGlobals) -> None:
    def _doexit() -> None:
        raise SystemExit()

    g.doexit = _doexit

    with route_signals_to_fd() as sigfd, selectors.DefaultSelector() as sel:
        sel.register(iomgr.CURSES_STDIN_FD, selectors.EVENT_READ, data=None)
        sel.register(sigfd, selectors.EVENT_READ, data=None)
        try:
            log.kpi("serving epoll")
            if __debug__ and g.opts.PROFILE_STARTUP:
                return
            while True:
                for key, events in sel.select():
                    if key.fd == sigfd:
                        assert events == selectors.EVENT_READ
                        handle_SIGWINCH(sel, sigfd, g)
                    elif key.fd == iomgr.CURSES_STDIN_FD:
                        assert events == selectors.EVENT_READ
                        g.curses_ui.handle_input()
                    else:
                        log.error(str((key, events)))
        except KeyboardInterrupt:
            pass
