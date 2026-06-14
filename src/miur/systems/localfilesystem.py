import os
from collections import OrderedDict
from sys import getsizeof
from time import monotonic_ns
from typing import TYPE_CHECKING, NamedTuple, Protocol

if TYPE_CHECKING:

    class IKernel(Protocol):
        pass


type FDInt = int
type HPath = str  # PERF:BET? use "bytes"


class ListdirProxy(NamedTuple):
    ts: int
    dt: int
    dlst: list[str]
    memsz: int
    st_ino: int
    st_dev: int
    st_size: int
    st_mtime_ns: int


class LocalFileSystem:
    # __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel

        self._listdir_cache: OrderedDict[HPath, ListdirProxy] = OrderedDict()
        self._listdir_total: int = 0
        self._listdir_maxsz = 100 * 1024 * 1024

    def listdir(self, h: HPath) -> list[str]:
        fd = os.open(h, os.O_RDONLY | os.O_DIRECTORY)
        try:
            # RND:(early bubble up to ui): FileNotFoundError,PermissionDenied
            st = os.fstat(fd)
            # WARN: keep in same try-catch (inof "else:") for os.scandir exceptions
            if lp := self._listdir_cache.get(h):
                was_replaced = st.st_ino != lp.st_ino or st.st_dev != lp.st_dev
                was_resized = st.st_size != lp.st_size
                maybe_modified = st.st_mtime_ns != lp.st_mtime_ns
                if was_replaced or was_resized or maybe_modified:
                    lp = self._listdir_new(h, fd, st)
                else:
                    self._listdir_cache.move_to_end(h)  # LRU: Mark as recently used
            else:
                lp = self._listdir_new(h, fd, st)
        finally:
            os.close(fd)
        return lp.dlst

    def _listdir_new(self, h: HPath, fd: FDInt, st: os.stat_result) -> ListdirProxy:
        t0 = monotonic_ns()
        with os.scandir(fd) as it:
            dlst = [x.name for x in it]
        t1 = monotonic_ns()

        lp = ListdirProxy(
            ts=t1,
            dt=t1 - t0,
            dlst=dlst,
            memsz=sum(getsizeof(s) for s in dlst),
            st_ino=st.st_ino,
            st_dev=st.st_dev,
            st_size=st.st_size,
            st_mtime_ns=st.st_mtime_ns,
        )
        memsz = getsizeof(lp) + sum(getsizeof(e) for e in lp) + lp.memsz
        while self._listdir_total + memsz > self._listdir_maxsz:
            _hX, lpX = self._listdir_cache.popitem(last=False)
            self._listdir_total -= lpX.memsz
            del lpX

        lp = lp._replace(memsz=memsz)
        self._listdir_cache[h] = lp
        self._listdir_total += memsz

        return lp
