###%USAGE: deferred evaluation
#  log.warning(lambda: f"{some}")

# ALT? from just.ext.logging import logcfg, L

import enum
import sys
import time
from typing import Any, Callable, TypeAlias

_Loggable: TypeAlias = Any
_LAMBDA = lambda: ""  # pylint:disable=unnecessary-lambda-assignment


@enum.unique
class LogLevel(enum.IntEnum):
    # FATAL = 90  # EMERGENCY, ABORT
    # CRITICAL = 70  # +ALERT
    ERROR = 50
    WARNING = 40
    INFO = 30  # +NOTICE | SUCCESS = 25
    # DEBUG = 25
    # OPTIONAL = 20
    # VERBOSE = 15  # DETAILED = 10
    TRACE = 10  # OR? =KPI  // :PERF: latency/cpu/memory
    # +TR1..TR9 (USE: gradually dimming gray colors, darker than .INFO)
    ANYTHING = 0  # OR: lvl=None


# TODO: make it a part of supplied .write(sys.stdout/stderr)
TERMSTYLE = {
    # LogLevel.CRITICAL: "\033[1;37m;41m",  # bold-white-on-red
    LogLevel.ERROR: "\033[31m",  # regul-red-on-dfl
    # LogLevel.ALERT: "\033[91m",  # regul-orange-on-dfl
    LogLevel.WARNING: "\033[33m",  # yellow
    LogLevel.INFO: "\033[m",  # dfl-on-dfl (none)
    # LogLevel.SUCCESS|TRACE: "\033[32m",  # green
    # LogLevel.NOTICE: "\033[34m",  # blue
    # LogLevel.DEBUG: "\033[95m",  # purple
    # LogLevel.VERBOSE: "\033[36m",  # cyan
    LogLevel.TRACE: "\033[36m",  # cyan
    # LogLevel.DETAILED: "\033[93m",  # grey
    None: "\033[m",  # none
}


class Logger:
    minlevel: LogLevel = LogLevel.ANYTHING
    write: Callable[[str], Any] = sys.stdout.write
    # TODO: make it a part of supplied .write()
    termcolor: bool = True

    def __init__(self) -> None:
        self._initts = time.monotonic()
        self._counter = 0

    def config(self, /, **kw: Any) -> None:
        for k, v in kw.items():
            # if type(getattr(self, k)) is not type(v):
            #     raise TypeError(type(getattr(self, k)))
            if not hasattr(self, k):
                raise KeyError(k)
            setattr(self, k, v)

    def error(self, fmt: _Loggable) -> None:
        self.at(LogLevel.ERROR, fmt)

    def warning(self, fmt: _Loggable) -> None:
        # ALT: LogLevel[sys._getframe().f_code.co_name.upper()]
        self.at(LogLevel.WARNING, fmt)

    def info(self, fmt: _Loggable) -> None:
        self.at(LogLevel.INFO, fmt)

    def trace(self, fmt: _Loggable) -> None:
        self.at(LogLevel.TRACE, fmt)

    def kpi(self, fmt: _Loggable) -> None:
        cpu = time.process_time() * 1000
        ms = (time.monotonic() - self._initts) * 1000
        self.at(LogLevel.TRACE, f"KPI({ms=:.3f} {cpu=:.3f}) {fmt}")

    # @profileit  # BAD: ~1ms/call (mostly due to !curses.*)
    def at(self, lvl: LogLevel, fmt: _Loggable) -> None:
        if lvl < self.minlevel:
            return
        if isinstance(fmt, str):
            body = fmt
        elif type(fmt) is type(_LAMBDA) and fmt.__name__ == _LAMBDA.__name__:
            body = fmt()
        else:
            body = str(fmt)
        relts = time.monotonic() - self._initts
        self._counter += 1

        # TODO: use lru_cache (src/mod/fcnnm/lnum) based on parent loci
        fr = sys._getframe(2)  # pylint:disable=protected-access
        # fcnnm = co.co_name  # co.co_qualname

        # ALT: modnm = sys._getframemodulename(2).rpartition('.')[2]
        # BET? modnm = fr.f_code.co_filename.rpartition("/")[2].rpartition(".")[0]
        # modnm = sys.modules[fr.f_globals['__name__']].__name__
        modnm = fr.f_globals["__name__"].rpartition(".")[2]
        lnum = fr.f_lineno

        if self.termcolor:
            _c = TERMSTYLE[lvl]
            _r = TERMSTYLE[None]
        else:
            _c = _r = ""
        # ADD? "#{self._counter:03d} ..."
        self.write(
            f"{relts:8.3f}  {_c}{lvl.name[0]}{_r}[{modnm}:{lnum}] {_c}{body}{_r}\n"
        )


log = Logger()
