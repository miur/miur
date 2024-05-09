###%USAGE: deferred evaluation
#  log.warning(lambda: f"{some}")

import sys
import enum
import time
from typing import Any, Callable


@enum.unique
class LogLevel(enum.IntEnum):
    # FATAL = 90  # EMERGENCY, ABORT
    # CRITICAL = 70  # +ALERT
    ERROR = 50
    WARNING = 40
    INFO = 30  # +NOTICE | SUCCESS = 25
    # DEBUG = 20
    # VERBOSE = 15
    # DETAILED = 10
    # TRACE = TR1..TR9 =
    ANYTHING = 0


TERMSTYLE = {
    # LogLevel.CRITICAL: "\033[1;37m;41m",  # bold-white-on-red
    LogLevel.ERROR: "\033[31m",  # regul-red-on-dfl
    # LogLevel.ALERT: "\033[91m",  # regul-orange-on-dfl
    LogLevel.WARNING: "\033[33m",  # yellow
    LogLevel.INFO: "\033[m",  # dfl-on-dfl (none)
    # LogLevel.SUCCESS|TRACE: "\033[32m",  # green
    # LogLevel.NOTICE: "\033[34m",  # blue
    # LogLevel.DEBUG: "\033[95m",  # purple
    # LogLevel.VERBOSE: "\033[37m",  # cyan
    # LogLevel.DETAILED: "\033[93m",  # grey
    None: "\033[m",  # none
}


class Logger:
    minlevel: LogLevel = LogLevel.INFO
    write: Callable[[str], None] = sys.stdout.write

    def __init__(self) -> None:
        self._initts = time.monotonic()

    def config(self, /, **kw: Any) -> None:
        for k, v in kw.items():
            # if type(getattr(self, k)) is not type(v):
            #     raise TypeError(type(getattr(self, k)))
            if not hasattr(self, k):
                raise KeyError(k)
            setattr(self, k, v)

    def warning(self, fmt: str | Callable[[], str]) -> None:
        # ALT: LogLevel[sys._getframe().f_code.co_name.upper()]
        self.at(LogLevel.WARNING, fmt)

    def info(self, fmt: str | Callable[[], str]) -> None:
        self.at(LogLevel.INFO, fmt)

    # @profile
    def at(self, lvl: LogLevel, fmt: str | Callable[[], str]) -> None:
        if lvl < self.minlevel:
            return
        if isinstance(fmt, str):
            body = fmt
        elif callable(fmt):
            body = fmt()
        else:
            raise NotImplementedError
        relts = time.monotonic() - self._initts

        fr = sys._getframe(2)  # pylint:disable=protected-access
        co = fr.f_code
        # fcnnm = co.co_name  # co.co_qualname
        srcnm = co.co_filename
        # ALT: sys._getframemodulename(2).rpartition('.')[2]
        modnm = srcnm.rpartition("/")[2].rpartition(".")[0]
        lnum = fr.f_lineno

        _c = TERMSTYLE[lvl]
        _r = TERMSTYLE[None]
        self.write(
            f"{relts:8.3f}  {_c}{lvl.name[0]}{_r}[{modnm}:{lnum}] {_c}{body}{_r}\n"
        )


log = Logger()
