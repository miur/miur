###%USAGE: deferred evaluation
#  log.warning(lambda: f"{some}")

# ALT? from just.ext.logging import logcfg, L

import enum
import sys
import time
from typing import Callable, Final, TypeAlias, TypedDict, Unpack

_LAMBDA: Final = lambda: ""  # pylint:disable=unnecessary-lambda-assignment
_Loggable: TypeAlias = str | Callable[[], str]


@enum.unique
class LogLevel(enum.IntEnum):
    # FATAL = 90  # EMERGENCY, ABORT
    # CRITICAL = 70  # +ALERT
    ERROR = 50
    WARNING = 40
    INFO = 30  # +NOTICE | SUCCESS = 25
    # DEBUG = 25
    # OPTIONAL = 20
    VERBOSE = 15  # DETAILED = 10
    TRACE = 10  # OR? =KPI  // :PERF: latency/cpu/memory
    # +TR1..TR9 (USE: gradually dimming gray colors, darker than .INFO)
    ANYTHING = 0  # OR: lvl=None


# TODO: make it a part of supplied .write(sys.stdout/stderr)
TERMSTYLE: Final = {
    # LogLevel.CRITICAL: "\033[1;37m;41m",  # bold-white-on-red
    LogLevel.ERROR: "\033[31m",  # regul-red-on-dfl
    # LogLevel.ALERT: "\033[91m",  # regul-orange-on-dfl
    LogLevel.WARNING: "\033[33m",  # yellow
    LogLevel.INFO: "\033[m",  # dfl-on-dfl (none)
    # LogLevel.SUCCESS|TRACE: "\033[32m",  # green
    # LogLevel.NOTICE: "\033[34m",  # blue
    # LogLevel.DEBUG: "\033[95m",  # purple
    LogLevel.VERBOSE: "\033[36m",  # cyan
    LogLevel.TRACE: "\033[36m",  # cyan
    # LogLevel.DETAILED: "\033[93m",  # grey
    None: "\033[m",  # none
}


## HACK: convert @dataclass to TypedDict -- to inherit baseclass from it
# ERR: TypedDict() expects a dictionary literal as the second argument
#   OFF: https://github.com/python/mypy/issues/4128
# _LoggerOpts = TypedDict('_LoggerOpts', {x.name: x.type for x in fields(LoggerState)}, total=False)
class _LoggerOpts(TypedDict, total=False):
    minlevel: LogLevel
    write: Callable[[str], None | int]
    termcolor: bool


class Logger:  # pylint:disable=too-many-instance-attributes
    minlevel: LogLevel = LogLevel.ANYTHING
    write: Callable[[str], None | int]
    termcolor: bool | None = None

    def __init__(self) -> None:
        # HACK: approximate ps creation time (w/o IO delays)
        self._initts = time.monotonic() - time.thread_time()
        self._pts = self._initts
        self._counter = 0
        self._pms = 0.0
        self._fnmlen = 0  # HACK: for gradual loglines indent alignment
        self._pcpu = time.process_time()
        super().__init__()
        self.at = self._lazy_init

    def _lazy_init(self, lvl: LogLevel, fmt: _Loggable) -> None:
        # NOTE: update to redirected FD
        if not hasattr(self, "write"):
            self.write = sys.stdout.write
        # TODO: make it a part of supplied .write()
        if self.termcolor is None:
            self.termcolor = sys.stdout.isatty()
        self.at = self._write
        self.at(lvl, fmt)

    # @profileit  # BAD: ~1ms/call (mostly due to !curses.*)
    def _write(self, lvl: LogLevel, fmt: _Loggable) -> None:
        if lvl < self.minlevel:
            return
        self.write(self._format(lvl, fmt))
        # MAYBE:ALSO: unified logging for lttng
        # if lvl == LogLevel.TRACE: sys.audit('log.trace', body)

    def config(self, /, **kw: Unpack[_LoggerOpts]) -> None:
        for k, v in kw.items():
            # if type(getattr(self, k)) is not type(v):
            #     raise TypeError(type(getattr(self, k)))
            if not hasattr(self, k) and k not in ("write"):
                raise KeyError(k)
            # if k == 'termcolor' and v is None:
            #     v = self.write.__self__.isatty()
            setattr(self, k, v)

    def error(self, fmt: _Loggable) -> None:
        self.at(LogLevel.ERROR, fmt)

    def warning(self, fmt: _Loggable) -> None:
        # ALT: LogLevel[sys._getframe().f_code.co_name.upper()]
        self.at(LogLevel.WARNING, fmt)

    def info(self, fmt: _Loggable) -> None:
        self.at(LogLevel.INFO, fmt)

    def verbose(self, fmt: _Loggable) -> None:
        self.at(LogLevel.VERBOSE, fmt)

    def trace(self, fmt: _Loggable) -> None:
        self.at(LogLevel.TRACE, fmt)

    def sep(self, fmt: _Loggable | None = None) -> None:
        """Separator with timestamps -- useful as 1st line in _live() Jupyter sessions"""
        line = "\n----- " + time.strftime("[%Y%m%d_%H%M%S] -----", time.localtime())
        if fmt:
            line += " " + str(fmt)
        self.at(LogLevel.TRACE, line)

    def kpi(self, fmt: _Loggable) -> None:
        ## PERF: to run startup 100 times and calc average
        # self.write("%.3f\n" % ((time.monotonic() - self._initts) * 1000))
        # return
        cpu = time.process_time()
        ms = time.monotonic() - self._initts
        dms = ms - self._pms
        dcpu = cpu - self._pcpu
        line = f"KPI[ms={ms*1000:.3f}({dms*1000:+.3f}) cpu={cpu*1000:.3f}({dcpu*1000:+.3f})] {fmt}"
        self.at(LogLevel.TRACE, line)
        # HACK: force show last KPI before exit
        # self.write.__self__.flush()
        self._pms = ms
        self._pcpu = cpu
        ## OLD
        # cpu = time.process_time() * 1000
        # ms = (time.monotonic() - self._initts) * 1000
        # self.at(LogLevel.TRACE, f"KPI({ms=:.3f} {cpu=:.3f}) {fmt}")

    def _format(self, lvl: LogLevel, fmt: _Loggable) -> str:
        if isinstance(fmt, str):
            body = fmt
        elif type(fmt) is type(_LAMBDA) and fmt.__name__ == _LAMBDA.__name__:
            body = fmt()
        else:
            body = str(fmt)
        ts = time.monotonic()
        relts = ts - self._initts
        # dts = ts - self._pts
        # self._pts = ts
        # self._counter += 1

        # TODO: use lru_cache (src/mod/fcnnm/lnum) based on parent loci
        fr = sys._getframe(2)  # pylint:disable=protected-access
        # NOTE: discover caller frame (outside of this file)
        while pr := fr.f_back:
            if fr.f_code.co_filename != __file__:
                break
            fr = pr
        # fcnnm = co.co_name  # co.co_qualname

        # ALT: modnm = sys._getframemodulename(2).rpartition('.')[2]
        # BET? modnm = fr.f_code.co_filename.rpartition("/")[2].rpartition(".")[0]
        # modnm = sys.modules[fr.f_globals['__name__']].__name__
        modnm = fr.f_globals["__name__"].rpartition(".")[2]
        lnum = fr.f_lineno
        loci = f"[{modnm}:{lnum}]"
        if len(loci) > self._fnmlen:
            self._fnmlen = len(loci)

        if self.termcolor:
            _c = TERMSTYLE[lvl]
            _b = "\033[92m"
            _r = TERMSTYLE[None]
        else:
            _c = _b = _r = ""
        # ADD? "#{self._counter:03d} {dts:+6.3f} ..."
        return f"{relts:8.3f}  {_c}{lvl.name[0]}{_b}{loci:<{self._fnmlen}s} {_c}{body}{_r}\n"


log = Logger()
