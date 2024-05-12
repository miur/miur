import curses as C
import os
import selectors
import signal
import sys
from contextlib import contextmanager  # closing  ExitStack,
from typing import Any, Final, Iterator

from . import curses_ext as CE
from .curses_cmds import input_handlers
from .util.log import log
from .util.exc import custom_excepthook_log
# from .util.sig import route_signals_to_fd


def mainloop(stdscr: C.window) -> None:
    # [_] FIXME: restore in "finally" to prevent logging to curses after it exits
    log.config(write=lambda text: CE.print_curses_altscreen(stdscr, text))

    if not C.has_extended_color_support():
        raise NotImplementedError

    # stdscr.refresh()

    # route_signals_to_fd() as sigfd,
    with selectors.DefaultSelector() as sel:
        sel.register(sys.stdin.fileno(), selectors.EVENT_READ, data=None)
        # sel.register(sigfd, selectors.EVENT_READ, data=None)
        stdscr.nodelay(True)
        try:
            while True:
                for key, events in sel.select():
                    # if key.fd == sigfd:
                    #     assert events == selectors.EVENT_READ
                    #     sig = os.read(sigfd, 1)
                    #     log.info(sig)
                    if key.fd == sys.stdin.fileno():
                        assert events == selectors.EVENT_READ
                        wch = stdscr.get_wch()
                        cmd = input_handlers.get(wch, None)
                        comment = f" ({cmd.__name__})" if cmd else ""
                        log.warning(repr(wch) + comment)
                        if cmd:
                            # WARN: last stmt in loop COS: may raise SystemExit
                            cmd(stdscr)
                    else:
                        log.error((key, events))
        except KeyboardInterrupt:
            pass
        finally:
            stdscr.nodelay(False)


# class Application:
#     @C.wrapper
#     def __enter__(self) -> Self:
#         return self
#
#     def __exit__(self, *exc_details) -> bool:
#         return self._stack.__exit__(*exc_details)


# TBD: frontend to various ways to run miur API with different UI
def miur(arg: str) -> None:
    # CHG: XDG_RUNTIME_DIR=/run/user/1000 + /miur/pid
    pidfile: Final = "/t/miur.pid"

    if arg.startswith("-SIG"):
        # MAYBE: check by short LOCK_EX if any LOCK_SH present (i.e. main miur running)
        try:
            # SEIZE: trbs/pid: Pidfile featuring stale detection and file-locking ⌇⡦⠿⣢⢔
            #   https://github.com/trbs/pid
            with open(pidfile, "r", encoding="utf-8") as f:
                pid = int(f.read())
        except FileNotFoundError as exc:
            log.error(f"fail {pidfile=} | {exc}")
            sys.exit(1)

        log.warning(f"sending signal={arg} to {pid}")
        try:
            os.kill(pid, getattr(signal, arg[1:]))
        except ProcessLookupError as exc:
            log.error(f"fail {pid=} | {exc}")
            sys.exit(1)
        sys.exit(0)

    lvl = int(os.environ.get("MIUR_LEVEL", "0"))
    if lvl != 0:
        log.error(f"avoid nesting {lvl=}")
        sys.exit(1)
    os.environ["MIUR_LEVEL"] = str(lvl + 1)

    # IDEA: force-notify epoll as if new data arrived inof actual fd
    signal.signal(signal.SIGWINCH, lambda si,fr:
        log.warning(f"{signal.Signals(si).name}={si}: {signal.strsignal(si)} during <{fr.f_code.co_filename}:{fr.f_lineno}>"))

    with open(pidfile, "w", encoding="utf-8") as f:
        f.write(str(os.getpid()))
    try:
        # with Application() as app:
        log.info(f"cwd={arg}")
        with custom_excepthook_log():
            return C.wrapper(mainloop)
    finally:
        os.remove(pidfile)


def _live() -> None:
    pass
