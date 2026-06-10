import enum
import time
from collections import deque
from typing import TYPE_CHECKING, Literal, NamedTuple, Protocol

if TYPE_CHECKING:

    class IKernel(Protocol):
        pass


class LogLevel(enum.IntEnum):
    E = ERROR = 50
    W = WARNING = 45
    I = INFO = 30
    D = DEBUG = 25


class LogEntry(NamedTuple):
    ts: int
    lvl: LogLevel
    obj: object

    def __str__(self) -> str:
        return f"{self.ts / 1e9:8.3f}  {self.lvl.name[0]}  {self.obj}\n"


# MAYBE: use sys.getsizeof() for backtrace frames locals capture
class LogSystem:
    def __init__(self, kernel: IKernel | None) -> None:
        self._initts = time.monotonic_ns()
        self.k = kernel
        self.ringbuffer: deque[LogEntry] = deque(maxlen=100)

    def info(self, obj: object) -> None:
        self(LogLevel.INFO, obj)

    def dump(self) -> str:
        # WARN: float may lose .ms precision for large .ts TRY:USE: (ts//1e9, ts%1e9//1e6)
        return "".join(map(str, self.ringbuffer))

    def __call__(self, lvl: int | Literal["E", "W", "I"], obj: object) -> None:
        if not isinstance(lvl, int):
            lvl = LogLevel[lvl]
        ts = time.monotonic_ns() - self._initts
        # MAYBE: keep object as-is until dump() -- but run lambda() immediately here
        #   BUT: class objects are mutable, so we may need deepcopy() unless its primitive type
        #     WARN: large arrays/dicts need compactization/rasterization
        # MAYBE: compact repeated entries to (repeated N times) ? but I will lose timestamps...
        #   ~~ TRY? could store obj=[ts1,ts2,...] to rasterize timestamps, though it's not ideal either
        entry = LogEntry(ts, lvl=lvl, obj=str(obj))
        self.ringbuffer.append(entry)


# HACK: until kernel=MiurKernel() -- all logs are "early logs"
log = LogSystem(None)
