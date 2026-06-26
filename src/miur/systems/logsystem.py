import enum
import queue
import sys
import time
from threading import get_native_id
from typing import TYPE_CHECKING, Literal, NamedTuple, Protocol, override

# import resource
# import tracemalloc

if TYPE_CHECKING:

    class IKernel(Protocol):
        pass


class LogLevel(enum.IntEnum):
    F = FAILURE = 90  # FATAL, EMERGENCY, ABORT
    # X = CRITICAL = 70  # +ALERT
    E = ERROR = 50
    W = WARNING = 45
    P = PERF = 42  # OR? =KPI  // :PERF: latency/cpu/memory
    # STATE = 35  # STATUS | ARGS | STARTUP // env/pwd/args/etc. (crucial exec ctx)
    # HAPPEN = 32  # ACTION|NORMAL|FLOW|HAPPYPATH|SEQ  // keypress, tasklet, events, comm
    I = INFO = 30  # +NOTICE | SUCCESS = 25
    N = NOTE = 28  # :TIP
    D = DEBUG = 25
    # OPTIONAL = 20
    V = VERBOSE = 15  # DETAILED = 10
    # TRACE = 10  # +TR1..TR9 (USE: gradually dimming gray colors, darker than .INFO)
    # COMMENT = 2
    # ANYTHING = 0  # OR: lvl=None


def colorscheme(enabled: bool = True) -> dict[str, str]:
    if not enabled:
        from collections import defaultdict

        return defaultdict(str)

    from ..uicommon.ansicolor import RESET, ansicolor

    return {
        "F": ansicolor(15, 1, bold=True),  # bold-white-on-red
        "E": ansicolor(1, bold=True),  # regul-red-on-dfl
        # "A": ansicolor(9),  # regul-orange-on-dfl
        "W": ansicolor(3),  # yellow
        "P": ansicolor(4, italic=True),  # green or blue
        # "S": ansicolor(4),  # green or blue
        "I": ansicolor(6),  # cyan
        # LogLevel.SUCCESS|TRACE: ansicolor(2),  # green
        "N": ansicolor(2),  # green
        "D": ansicolor(13),  # purple
        "V": ansicolor(6),  # cyan
        # "T": ansicolor(-1),  # dfl-on-dfl (none)
        "tid": ansicolor(0),
        "b": ansicolor(10),
        "": RESET,
    }


class LogEntry(NamedTuple):
    ts: int
    lvl: LogLevel
    tid: int
    modnm: str
    lnum: int
    obj: object


def calibrate_clock_drift(samples: int = 20) -> int:
    """Calculates the drift offset between time.time_ns() and time.monotonic_ns().
    Formula: epoch_ns = monotonic_ns + drift_offset
    """
    best_window = float("inf")
    best_drift = 0
    for _ in range(samples):  # 1. Sample in a tight sequence
        m1 = time.monotonic_ns()
        # ATT: must use time.time() because create_time() is wall-clock based
        t = time.time_ns()
        m2 = time.monotonic_ns()
        # 2. Calculate the execution window size (jitter)
        window = m2 - m1
        # 3. Keep the sample with the least amount of OS interruption
        if window < best_window:
            best_window = window
            # Use the midpoint of monotonic samples to align with the center 't'
            m_midpoint = (m1 + m2) // 2
            best_drift = t - m_midpoint
    return best_drift


# MAYBE: use sys.getsizeof() for backtrace frames locals capture
class LogSystem:  # pylint:disable=too-many-instance-attributes
    def __init__(self, kernel: IKernel | None) -> None:
        self.k = kernel
        self.minlevel = LogLevel.VERBOSE
        self.enablecolors = True
        self._init_tid = get_native_id()

        ## DISABLED:(deque->list): needed a way to dump only fresh logs after last dump()
        # self.ringbuffer: deque[LogEntry] = deque(maxlen=100)
        # self.ringbuffer: list[LogEntry] = []
        ## ALT: deque(atomic) with Lock() for safe iteration
        # INFO: Written entirely in C, ultra-fast lock handling (put=120ns)
        self._queue_threadsafe: queue.SimpleQueue[LogEntry] = queue.SimpleQueue()
        self._archive: list[LogEntry] = []
        self._measurements: list[tuple[str, int]] = []  # RENAME? _perfpoints
        self._measured_until: int = 0
        self._colors: dict[str, str] = {}

        import psutil

        self.process = psutil.Process()
        epoch_ns = int(self.process.create_time() * 1e9)
        drift_ns = calibrate_clock_drift()
        self._prevkpi_ts = self._appstart_ts = epoch_ns - drift_ns
        self._prevkpi_cpu = time.process_time_ns()

        # tracemalloc.start()  # OR: -X tracemalloc
        # tracemalloc.is_tracing()
        # tracemalloc.stop()

    def archive_recent(self, limit: int = 0, dump: bool = False) -> str:
        ar = self._archive
        prevlen = len(ar)
        archive = ar.append
        getlog = self._queue_threadsafe.get_nowait
        try:
            # HACK:(!=0 inof >0): unbound when limit=0
            while (limit := limit - 1) != 0:
                archive(getlog())
        except queue.Empty:
            pass
        return self.dump_range(prevlen) if dump else ""

    @override
    def __str__(self) -> str:
        self(LogLevel.INFO, f"Nlogs={len(self._archive)}")
        self.archive_recent(dump=False)
        return self.dump_range()

    def dump_range(self, start: int = 0, end: int = 0) -> str:
        if not self._colors:
            self._colors = colorscheme(self.enablecolors)

        fmt = self._format
        ar = self._archive
        if start or end:
            return "".join(fmt(ar[i]) for i in range(start, end or len(ar)))
        return "".join(map(fmt, self._archive))

    def _format(self, e: LogEntry, /) -> str:
        c = self._colors
        # WARN: intermediate float value may lose .ms precision for large .ts
        #   TRY:USE: tuple(ts//1e9, ts%1e9//1e6)
        # ADD? "#{self._counter:03d} {dts:+6.3f} ..."
        lvl = e.lvl.name[0]
        tid = f"<{e.tid}>" if e.tid != self._init_tid else ""
        return (
            f"{e.ts / 1e9:8.3f} {tid} "
            f"{c[lvl]}{lvl}{c['b']}{f'[{e.modnm}:{e.lnum}]':<{12}s}"
            f" {c[lvl]}{e.obj}{c['']}\n"
        )

    # MAYBE? allow [*aobj, **kobj] and store all of them to rasterize later?
    def __call__(
        self, lvl: Literal["F", "E", "W", "I", "D", "V"] | LogLevel, obj: object
    ) -> None:
        if isinstance(lvl, str):
            lvl = LogLevel[lvl]
        if lvl < self.minlevel:
            return

        fr = sys._getframe(1)  # pylint:disable=protected-access  # pyright:ignore[reportPrivateUsage]
        while pr := fr.f_back:
            if fr.f_code.co_filename != __file__:
                break
            fr = pr

        # MAYBE: keep object as-is until dump() -- but run lambda() immediately here
        #   BUT: class objects are mutable, so we may need deepcopy() unless its primitive type
        #     WARN: large arrays/dicts need compactization/rasterization
        # [_] IDEA! inof deferred lambda use template-strings resolved by upper frame locals()
        #   _LAMBDA: Final = lambda: ""  # pylint:disable=unnecessary-lambda-assignment
        #   type Loggable = str | Callable[[], str] | object
        # MAYBE: compact repeated entries to (repeated N times) ? but I will lose timestamps...
        #   ~~ TRY? could store obj=[ts1,ts2,...] to rasterize timestamps, though it's not ideal either
        entry = LogEntry(
            ts=time.monotonic_ns() - self._appstart_ts,
            lvl=lvl,  # MAYBE? lvl=LogLevel.FAILURE if isinstance(obj, BaseException) else lvl
            tid=get_native_id(),
            modnm=fr.f_globals["__name__"].rpartition(".")[2],
            lnum=fr.f_lineno,
            # FIXME: use my legacy log_exc()
            #   CHECK:CMP: vs newest impl of std traceback printing
            obj=obj if isinstance(obj, BaseException) else str(obj),
        )
        self._queue_threadsafe.put(entry)

        ## TEMP:DEBUG: get immediate logs for crashes
        ##   BET: in multi_drv mode use first terminal for textstream -- and redirect curses to another terminal
        # if not self._colors:
        #     self._colors = colorscheme(self.enablecolors)
        # # TEMP: use "\r" to print traceback in curses.raw() without messing up layout
        # print(self._format(entry), end="\r")

    ## DISABLED: too complicated to avoid repeated declarations...
    ## ALT:PERF~ self(LogLevel[sys._getframe().f_code.co_name.upper()], ...)
    # class LogFn(Protocol):
    #     def __call__(self: LogSystem, *args: object) -> None: ...
    # class LogSystem:
    #     info: LogFn
    #     error: LogFn
    #     # HACK: Dynamically attach the methods to the class definition scope
    #     _ns = locals()
    #     for _lvl in LogLevel:
    #         _fnm = _lvl.name.lower() if len(_lvl.name) > 1 else _lvl.name.upper()
    #         # OR: locals()[lvl.name.lower()] = lambda self, *a, lvl=lvl: self(lvl, *a)
    #         @staticmethod
    #         def _mklog(lvl: LogLevel) -> LogFn:
    #             def _loglvl(self: LogSystem, *args: object) -> None:
    #                 self(lvl, *args)
    #             return _loglvl
    #         _ns[_fnm] = _mklog(_lvl)
    #     del _ns, _fnm, _mklog, _lvl

    def error(self, obj: object, /) -> None:
        self(LogLevel.ERROR, obj)

    def warning(self, obj: object, /) -> None:
        self(LogLevel.WARNING, obj)

    def info(self, obj: object, /) -> None:
        self(LogLevel.INFO, obj)

    def note(self, obj: object, /) -> None:
        self(LogLevel.NOTE, obj)

    def debug(self, obj: object, /) -> None:
        self(LogLevel.DEBUG, obj)

    def verbose(self, obj: object, /) -> None:
        self(LogLevel.VERBOSE, obj)

    # RENAME? refpt -> seqnm
    # MAYBE: allow additional :int args to e.g. calc avg "tokens"
    def measure(self, refpt: str, /) -> None:
        self._measurements.append((refpt, time.perf_counter_ns()))

    # TODO? calc FPS refresh rate for when key is pressed and held
    def recent_measurements_avg(self) -> str:
        from collections import defaultdict
        from itertools import islice

        deltas: dict[str, list[int]] = defaultdict(list)
        it = iter(islice(self._measurements, self._measured_until, None))
        k0, t0 = next(it)
        for k1, t1 in it:
            deltas[k0].append(t1 - t0)
            k0, t0 = k1, t1
        self._measured_until = len(self._measurements)
        return " ".join(f"{k}={sum(xt) / 1e6:.1f}ms" for k, xt in deltas.items())

    def kpi(self, obj: object, /) -> None:
        ## PERF: to run startup 100 times and calc average
        # self.write("%.3f\n" % ((time.monotonic() - self._initts) * 1000))
        # return
        st = ""
        dts = (ts := time.monotonic_ns()) - self._prevkpi_ts
        st += f"dts={dts / 1e6:+.2f}"
        self._prevkpi_ts = ts

        dcpu = (cpu := time.process_time_ns()) - self._prevkpi_cpu
        st += f" dcpu={dcpu / 1e6:+.2f}(Σ{cpu / 1e9:,.2f})"
        self._prevkpi_cpu = cpu

        # st += f" Nfd={self.process.num_fds()}"
        # peak_kb = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
        # st += f" RSS={self.process.memory_info().rss / (1 << 20):.1f}MB(peak={peak_kb / 1024:.1f}MB)"
        # curr, peak = tracemalloc.get_traced_memory()
        # st += f" malloc={curr // 1024:,}kB(peak={peak // 1024:,}kB)"

        self(LogLevel.PERF, f"{obj:16} // KPI[ {st} ]")


# HACK: until kernel=MiurKernel() -- all logs are "early logs"
log = LogSystem(None)
