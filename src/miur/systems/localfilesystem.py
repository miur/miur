import mmap
import os
from collections import OrderedDict
from typing import TYPE_CHECKING, Protocol

from .filecontent import FileContentProxy

if TYPE_CHECKING:

    class IKernel(Protocol):
        pass


type FDInt = int
type HPath = str


PAGE_SIZE = os.sysconf("SC_PAGE_SIZE")  # Usually 4096
MIN_ADVICE = 128 * 1024  # read-ahead for MADV_NORMAL
MAX_ADVICE = 2 * 1024 * 1024  # ~2..8MiB
NEXT_PREFETCH = 64 * 1024  # for smooth forward scroll


class LocalFileSystem:
    # __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel

        ### SPLIT? -> `FileContentCacheManagerSystem (separate from LocalFileSystem)
        self._fullcontent_cache: dict[HPath, bytes] = {}
        self._mmap_cache: OrderedDict[HPath, tuple[mmap.mmap, FDInt]] = OrderedDict()
        ## MOVE? GlobalPrefsSystem vs dynamically gather opts and virtually recombine from all systems
        # TODO:ENH: cache files <512kiB and reload them on change inof reopening mmap
        self._mmap_threshold_filesz = 512 * 1024
        # INFO Memory Maps: cat /proc/sys/vm/max_map_count (DFL=65530 for >400 files)
        # INFO Open Files: ulimit -n (Default is often 1024). Keep your cache_size safely below this.
        self._mmap_zones_maxnum = 100

    def listdir(self, h: HPath) -> list[str]:
        with os.scandir(h) as it:
            return [x.name for x in it]

    # BAD: where to cache mm/ino ? In OpenedFilesSystem to limit fd-open resources ?
    #   DECI: or combine with "scroll/view state" ?
    def file_content(self, h: HPath) -> FileContentProxy:
        return FileContentProxy(h)

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

    def read_bytes_safe(self, h: HPath, offset: int, length: int) -> bytes:
        # CASE: cached
        if bs := self._fullcontent_cache.get(h):
            return bs[offset : offset + length]
        if mmfd := self._mmap_cache.get(h):
            self._mmap_hint_os(mmfd[0], offset, length)
            self._mmap_cache.move_to_end(h)  # LRU: Mark as recently used
            return mmfd[0][offset : offset + length]

        path = h  # FUT? =k.get_interned_path(h)
        fd = os.open(path, os.O_RDONLY)
        st = os.stat(fd)

        # CASE: contents
        if (fullsz := st.st_size) < self._mmap_threshold_filesz:
            try:
                data = os.read(fd, fullsz)
                # NOTE: tell the kernel it can free its own cache copy immediately
                #   = free up the physical RAM, keeping only the virtual map.
                os.posix_fadvise(fd, 0, 0, os.POSIX_FADV_DONTNEED)
            finally:
                os.close(fd)
            self._fullcontent_cache[h] = data
            return data[offset : offset + length]

        # CASE: mmap
        if len(self._mmap_cache) >= self._mmap_zones_maxnum:
            _pathX, (mmX, fdX) = self._mmap_cache.popitem(last=False)
            # NOTE:(_evict_oldest): prevent hitting vm.max_map_count limit
            #   == "We don't need this whole file in RAM anymore"
            os.posix_fadvise(fdX, 0, 0, os.POSIX_FADV_DONTNEED)
            mmX.close()
            os.close(fdX)

        mm = mmap.mmap(fd, length=0, offset=0, access=mmap.ACCESS_READ)
        self._mmap_hint_os(mm, offset, length)
        self._mmap_cache[h] = (mm, fd)
        return mm[offset : offset + length]
