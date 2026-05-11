import fcntl
import mmap
import os
from typing import Iterator


class FileContentProxy:
    __slots__ = ("_path", "_fd", "_mm", "_st_ino", "_st_dev", "_aligned_offset")
    _fd: int  # ALT:(int): type FilDes = int
    _mm: mmap.mmap
    _aligned_offset: int

    def __init__(self, path: str) -> None:
        self._path = path
        self._st_ino = 0
        self._st_dev = 0
        # self._lock = threading.Lock()  # Internal thread safety

    def close(self) -> None:
        # with self._lock:
        if self._mm:
            self._mm.close()  # THINK: should I allow repeating .close() or prefer error?
        if self._fd:
            os.close(self._fd)

    # FUT? we may have alt-impl with .read() inof mmap()
    def _mmap_reopen(self, offset: int = 0) -> None:
        if getattr(self, "_mm", None):
            self._mm.close()
        if getattr(self, "_fd", None):
            os.close(self._fd)
        self._fd = os.open(self._path, os.O_RDONLY)
        granularity = mmap.ALLOCATIONGRANULARITY  # =PAGESIZE on Unix systems.
        self._aligned_offset = (offset // granularity) * granularity
        self._mm = mmap.mmap(
            self._fd,
            length=0,
            offset=self._aligned_offset,
            access=mmap.ACCESS_READ,
        )
        if skipped_padding := offset - self._aligned_offset:
            self._mm.seek(skipped_padding)
        st = os.stat(self._fd)  # WARN: don't reuse stat(path) due to reopen racing
        self._st_ino = st.st_ino
        self._st_dev = st.st_dev

    def _ensure_mmap(self, offset: int = 0) -> bool:
        # RND: bubble up FileNotFoundError to show it in UI
        st = os.stat(self._path)
        # WARN: If your file is on a Network File System (NFS), be aware that some NFS
        #   servers might reuse inode numbers very quickly after a file is deleted.
        # WARN: on Windows, st_ino is less reliable
        #   BUT recreation usually requires closing the handle anyway
        #   ALSO: on Windows file can't be deleted if mapped, but recreation can still occur
        if _file_is_replaced := st.st_ino != self._st_ino or st.st_dev != self._st_dev:
            self._mmap_reopen(offset)
            return True
        if _mmap_area_changed := st.st_size != self._mm.size():
            # WARN: on Windows you may need to always reopen() inof mm.resize()
            try:
                # ATT: Resizing will fail with an exception if any memoryview
                #   or buffer object currently references the mmap.
                self._mm.resize(st.st_size)
            except OSError, TypeError:
                # WARN: resizing a ACCESS_READ/COPY mmap will raise a TypeError exception.
                #   >> ALT:BAD?(unsafe): use ACCESS_WRITE even if you only intend to read
                # WARN:(on Windows): resizing the map will raise an OSError
                #   if there are other maps against the same named file.
                # INFO: resizing a map created with trackfd set to False, will raise a ValueError exception.
                self._mmap_reopen(offset)
                return True
        return False

    def __iter__(self) -> Iterator[object]:
        return self.read_lines()

    def __getitem__(self, kx: slice, /) -> list[str]:
        linebegoff = 0 if kx.start is None else self.seek_to_line_nth(kx.start)
        lnum = (kx.stop or 0) - (kx.start or 0)
        return list(self.read_lines(lnum, offset=linebegoff))

    def seek_to_line_nth(self, lnum: int, relative_to_offset: int = 0) -> int:
        self._ensure_mmap()
        cur = relative_to_offset - self._aligned_offset
        find = self._mm.find
        for _ in range(lnum):
            end = find(b"\n", cur)
            if end == -1:
                return len(self._mm)  # CHG? eof OR raise Error
            cur = end + 1
        return cur

    def read_bytes_safe(self, offset: int, length: int) -> bytes | None:
        # MAYBE: with self._lock:
        try:
            ## ALT:(Unix + Windows): pip install portalocker
            # with portalocker.Lock(self.path, mode="rb", flags=portalocker.LOCK_SH | portalocker.LOCK_NB):
            # ... except portalocker.exceptions.LockException: return None
            ### WARN: On some networked filesystems it might be needed to force a os.fsync() before closing
            ###   the file so it’s actually written before another client reads the
            ## with portalocker.Lock('some_file', 'rb+', timeout=60) as fh:
            ##     fh.write(...); fh.flush(); os.fsync(fh.fileno())
            fcntl.flock(self._fd, fcntl.LOCK_SH | fcntl.LOCK_NB)
            mapped_offset = offset - self._aligned_offset
            # ARCH:ATT: we are forced to call _ensure_mmap() in each function,
            #   because file may change externally and we may need remap
            self._ensure_mmap()  # OR: if not self.mm: return None
            try:
                ### Prevent out-of-bounds reads
                ## WARN:MAYBE:(anyway): try: ... except ValueError: return None
                #   WHY: mmap will raise this if you attempt to access an offset that
                #     became invalid because another process truncated the file.
                # end = min(offset + length, self._mm.size())
                # if offset >= self._mm.size():
                #     return b""
                return self._mm[mapped_offset : mapped_offset + length]
            finally:
                fcntl.flock(self._fd, fcntl.LOCK_UN)
        except BlockingIOError:
            return None  # File is locked by an exclusive writer

    def read_lines(self, num: int = 0, offset: int = 0) -> Iterator[str]:
        self._ensure_mmap()
        self._mm.seek(offset - self._aligned_offset)
        readline_bytes = self._mm.readline
        if not num:  # CASE: read till end or break from loop
            while lb := readline_bytes():
                yield lb.decode("utf-8")
            return
        for _ in range(num):
            if lb := readline_bytes():
                yield lb.decode("utf-8")
            else:
                break

    ## UNUSED:PERF~: use memoryview() *only* when line length is > [1kB..64kB]
    ##   <<WHY: it has 200 bytes overhead and per-byte indirection layer
    # FUT:PERF? return byte-lines as-is and directly render them in curses as bytes ?
    def read_lines_verylong(self, offset: int, num_lines: int = 100) -> list[str]:
        self._ensure_mmap()
        cur_pos = offset - self._aligned_offset
        mm = self._mm
        lines: list[str] = []
        assert mm
        with memoryview(mm) as mv:
            for _ in range(num_lines):
                # INFO: find() on mmap is fast; find() on memoryview is slow
                end_pos = mm.find(b"\n", cur_pos)
                if end_pos == -1:
                    if cur_pos < len(mm):
                        lines.append(str(mv[cur_pos:], encoding="utf-8"))
                    break
                # INFO: abs_byteoffset = aligned_offset + cur_pos
                lines.append(str(mv[cur_pos:end_pos], encoding="utf-8"))
                cur_pos = end_pos + 1
        # FIXME: store byte extent [beg:end] -- to scroll-wnd-up too
        mm.seek(cur_pos)  # REM? update for later .readline
        return lines
