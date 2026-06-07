import mmap
import os
from collections import OrderedDict
from typing import TYPE_CHECKING, Callable, NamedTuple, Protocol

from .localfilesystem import FDInt, HPath

if TYPE_CHECKING:

    class IKernel(Protocol):
        pass


## TODO: make two separate classes with classmethods!
# class ContentProxy[T = bytes | mmap.mmap](NamedTuple):
class ContentProxy[T](NamedTuple):
    st_ino: int
    st_dev: int
    st_size: int
    st_mtime_ns: int
    # ALT: make both .data and .mm with default values
    data: T
    fd: FDInt


PAGE_SIZE = os.sysconf("SC_PAGE_SIZE")  # Usually 4096
MIN_ADVICE = 1 * 1024  # read-ahead for MADV_NORMAL (HDD=<128kiB NVME=>512kiB)
MAX_ADVICE = 2 * 1024 * 1024  # (SSD=~2..8MiB NVME=>>2MiB)
NEXT_PREFETCH = 64 * 1024  # for smooth forward scroll


# RENAME? -> `LocalFileContentCacheManagerSystem/OpenedFilesSystem
class FileContentSystem:
    # __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel
        # self._lock = threading.Lock()  # Internal thread safety

        self._fullcontent_cache: OrderedDict[HPath, ContentProxy[bytes]] = OrderedDict()
        self._fullcontent_total: int = 0
        ## MOVE? GlobalPrefsSystem vs dynamically gather opts and virtually recombine from all systems
        self._fullcontent_maxsz = 200 * 1024 * 1024  # MiB

        self._mmap_cache: OrderedDict[HPath, ContentProxy[mmap.mmap]] = OrderedDict()
        self._mmap_threshold_filesz = 512 * 1024
        # INFO Memory Maps: cat /proc/sys/vm/max_map_count (DFL=65530 for >400 files)
        # INFO Open Files: ulimit -n (Default is often 1024). Keep your cache_size safely below this.
        self._mmap_zones_maxnum = 100
        # self._aligned_offset: int

    def find_fn(self, h: HPath) -> Callable[[bytes, int, int | None], int]:
        cp = self._ensure_cached(h, 0, 0, None)
        if not cp:
            raise RuntimeError()
        fd = cp.data.find
        return lambda s, b, e: fd(s, b) if e is None else fd(s, b, e)

    def size(self, h: HPath) -> int:
        cp = self._ensure_cached(h, 0, 0, None)
        if not cp:
            raise RuntimeError()
        if isinstance(cp.data, bytes):
            return len(cp.data)
        # WARN: len(mm): Returns the size of the currently memory-mapped buffer.
        #   If you resize the memory map, this value shifts to match the active window of bytes you can read.
        # * mm.size(): Returns the size of the underlying file on disk when the mmap was initially opened.
        return cp.data.size()

    def readline(self, h: HPath, offset: int) -> bytes:
        cp = self._ensure_cached(h, offset, 0, None)
        if not cp:
            raise RuntimeError()
        if isinstance(cp.data, bytes):
            nl = cp.data.find(b"\n", offset)
            return cp.data[offset : nl if nl != -1 else None]
        cp.data.seek(offset)  #  offset - self._aligned_offset
        return cp.data.readline()

    def read_bytes(
        self,
        h: HPath,
        offset: int,
        length: int,
        # RENAME?(refresh): latest, fromcache, skiprefresh
        refresh: bool | None = True,  # <PERF: avoid os.stat on each call
    ) -> bytes | None:
        if cp := self._ensure_cached(h, offset, length, refresh):
            return cp.data[offset : offset + length]
        if refresh is False:
            return None
        raise RuntimeError()

    def drop(self, h: HPath) -> None:  # RENAME? _{close,clear,drop,release}
        # with self._lock:
        # NOTE:(just in case): we remove from both caches
        if cp := self._fullcontent_cache.pop(h, None):
            self._fullcontent_total -= cp.st_size
        if cp := self._mmap_cache.pop(h, None):
            cp.data.close()  # THINK: should I allow repeating .close() or prefer error?
            os.posix_fadvise(cp.fd, 0, 0, os.POSIX_FADV_DONTNEED)
            os.close(cp.fd)

    def _ensure_cached(  # pylint:disable=too-many-branches,too-many-return-statements
        self,
        h: HPath,
        offset: int,
        length: int,
        refresh: bool | None,
    ) -> ContentProxy[bytes] | ContentProxy[mmap.mmap] | None:
        cp = self._fullcontent_cache.get(h) or self._mmap_cache.get(h)
        if refresh is False:
            if cp:
                if isinstance(cp.data, bytes):
                    self._fullcontent_cache.move_to_end(h)  # LRU: Mark as recently used
                else:
                    self._mmap_hint_os(cp.data, offset, length)
                    self._mmap_cache.move_to_end(h)  # LRU: Mark as recently used
            return cp

        if not cp or refresh is True:
            # CASE: grab
            path = h  # FUT? =k.get_interned_path(h)
            fd = os.open(
                path, os.O_RDONLY
            )  # WARN: grab fd to avoid unlink/rename racing
            try:
                st = os.stat(fd)  # WARN: don't reuse stat(path) due to reopen racing
            except:
                os.close(fd)
                raise  # RND:(early bubble up to ui): FileNotFoundError,PermissionDenied

            if not cp:
                # return False  # THINK: notify replacement or not?
                return self._fullread_or_mmap(h, fd, st, offset, length)

        if refresh is None:
            return cp

        # CASE: cached + invalidate
        # WARN: If your file is on a Network File System (NFS), be aware that some NFS
        #   servers might reuse inode numbers very quickly after a file is deleted.
        # WARN: on Windows, st_ino is less reliable
        #   BUT recreation usually requires closing the handle anyway
        #   ALSO: on Windows file can't be deleted if mapped, but recreation can still occur
        assert st and fd  # WTF:WHY: error: "st" (reportPossiblyUnboundVariable)
        was_replaced = st.st_ino != cp.st_ino or st.st_dev != cp.st_dev
        was_resized = st.st_size != cp.st_size
        maybe_modified = st.st_mtime_ns != cp.st_mtime_ns

        if isinstance(cp.data, bytes):
            if was_replaced or was_resized or maybe_modified:
                return self._fullread_or_mmap(h, fd, st, offset, length)
            self._fullcontent_cache.move_to_end(h)  # LRU: Mark as recently used
            return cp
        if not isinstance(cp.data, mmap.mmap):  # pyright: ignore[reportUnnecessaryIsInstance]
            raise ValueError(cp)

        if was_replaced:
            cp.data.close()
            os.posix_fadvise(cp.fd, 0, 0, os.POSIX_FADV_DONTNEED)
            os.close(cp.fd)
            return self._fullread_or_mmap(h, fd, st, offset, length)
        if was_resized:
            # WARN: on Windows you may need to always reopen() inof mm.resize()
            try:
                # ATT: Resizing will fail with an exception if any memoryview
                #   or buffer object currently references the mmap.
                cp.data.resize(st.st_size)
                return cp._replace(st_size=st.st_size, st_mtime_ns=st.st_mtime_ns)
            except OSError, TypeError:
                # WARN: resizing a ACCESS_READ/COPY mmap will raise a TypeError exception.
                #   >> ALT:BAD?(unsafe): use ACCESS_WRITE even if you only intend to read
                # WARN:(on Windows): resizing the map will raise an OSError
                #   if there are other maps against the same named file.
                # INFO: resizing a map created with trackfd=False, will raise a ValueError exception.
                cp.data.close()
                os.posix_fadvise(cp.fd, 0, 0, os.POSIX_FADV_DONTNEED)
                os.close(cp.fd)
                return self._fullread_or_mmap(h, fd, st, offset, length)

        # "~same" i.a. "was_modified"
        self._mmap_hint_os(cp.data, offset, length)
        self._mmap_cache.move_to_end(h)  # LRU: Mark as recently used
        return cp

    def _fullread_or_mmap(
        self, h: HPath, fd: FDInt, st: os.stat_result, offset: int, length: int
    ) -> ContentProxy[bytes] | ContentProxy[mmap.mmap]:
        if st.st_size < self._mmap_threshold_filesz:
            return self._fullcontent_read(h, fd, st)
        return self._mmap_open(h, fd, st, offset, length)

    def _fullcontent_read(
        self, h: HPath, fd: FDInt, st: os.stat_result
    ) -> ContentProxy[bytes]:
        try:
            data = os.read(fd, st.st_size)
            # NOTE: tell the kernel it can free its own cache copy immediately
            #   = free up the physical RAM, keeping only the virtual map.
            os.posix_fadvise(fd, 0, 0, os.POSIX_FADV_DONTNEED)
        finally:
            os.close(fd)

        while self._fullcontent_total + len(data) > self._fullcontent_maxsz:
            _hX, cpX = self._fullcontent_cache.popitem(last=False)
            self._fullcontent_total -= cpX.st_size
            del cpX

        cp = self._fullcontent_cache[h] = ContentProxy(
            st_ino=st.st_ino,
            st_dev=st.st_dev,
            st_size=st.st_size,
            st_mtime_ns=st.st_mtime_ns,
            data=data,
            fd=0,
        )
        self._fullcontent_total += cp.st_size
        return cp

    def _mmap_open(
        self, h: HPath, fd: FDInt, st: os.stat_result, offset: int, length: int
    ) -> ContentProxy[mmap.mmap]:
        if len(self._mmap_cache) >= self._mmap_zones_maxnum:
            _hX, cpX = self._mmap_cache.popitem(last=False)
            # NOTE:(_evict_oldest): prevent hitting vm.max_map_count limit
            #   == "We don't need this whole file in RAM anymore"
            os.posix_fadvise(cpX.fd, 0, 0, os.POSIX_FADV_DONTNEED)
            cpX.data.close()
            os.close(cpX.fd)

        # granularity = mmap.ALLOCATIONGRANULARITY  # =PAGESIZE on Unix systems.
        # self._aligned_offset = (offset // granularity) * granularity
        # offset=self._aligned_offset,
        mm = mmap.mmap(fd, length=0, offset=0, access=mmap.ACCESS_READ)
        self._mmap_hint_os(mm, offset, length)
        # if skipped_padding := offset - self._aligned_offset:
        #     self._mm.seek(skipped_padding)
        cp = self._mmap_cache[h] = ContentProxy(
            st_ino=st.st_ino,
            st_dev=st.st_dev,
            st_size=st.st_size,
            st_mtime_ns=st.st_mtime_ns,
            data=mm,
            fd=fd,
        )
        return cp

    @staticmethod
    def _mmap_hint_os(mm: mmap.mmap, offset: int, length: int) -> None:
        preloadsz = min(length + NEXT_PREFETCH, MAX_ADVICE)
        if preloadsz > MIN_ADVICE:
            ## CHECK: do I truly need to align myself ?
            # aligned_offset = (offset // PAGE_SIZE) * PAGE_SIZE
            # end_position = offset + slice_length
            # aligned_end = ((end_position + PAGE_SIZE - 1) // PAGE_SIZE) * PAGE_SIZE
            # aligned_length = aligned_end - aligned_offset
            ## OR: os.posix_fadvise(mmfd[1], offset, preloadsz, os.POSIX_FADV_WILLNEED)
            mm.madvise(mmap.MADV_WILLNEED, offset, preloadsz)
        ## TBD: cursor back-scrolling (micro-adjacements) -- of doubtful efficiency ?
        # if offset > NEXT_PREFETCH:
        #     os.posix_fadvise(fd, offset - NEXT_PREFETCH, NEXT_PREFETCH, os.POSIX_FADV_WILLNEED)
        ## ALT? may need _RANDOM to disable pre-fetch for individual items ?
