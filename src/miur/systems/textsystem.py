from typing import TYPE_CHECKING, Protocol

if TYPE_CHECKING:
    from .filecontentsystem import FileContentSystem

    class IKernel(Protocol):
        file: FileContentSystem


class TextSystem:
    __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel

    ### TBD:RFC! >>>>

    # def __iter__(self) -> Iterator[object]:
    #     return self.read_lines()

    # def __getitem__(self, kx: slice, /) -> list[str]:
    #     linebegoff = 0 if kx.start is None else self.seek_fwd_to_line_nth(kx.start)
    #     lnum = (kx.stop or 0) - (kx.start or 0)
    #     return list(self.read_lines(lnum, offset=linebegoff))

    def seek_fwd_to_line_nth(
        self, h: str, lnum: int, relative_to_offset: int = 0
    ) -> int:
        cur = relative_to_offset  # - self._aligned_offset
        find = self.k.file.find_fn(h)
        for _ in range(lnum):
            end = find(b"\n", cur)
            if end == -1:
                # return len(self._mm)  # OR: -1 | eof
                raise ValueError(lnum)
            cur = end + 1
        return cur

    # WARN: don't cache it -- mmap can be externally overwritten and change number of lines
    def count_lines(self, h: str, start: int = 0, end: int = 0) -> int:
        count = 0
        ppos = pos = start - 1  # - self._aligned_offset
        filesz = self.k.file.size(h)
        eom = end or min(10_000_000, filesz)  # (...) - self._aligned_offset
        assert -1 <= pos < eom <= filesz
        find = self.k.file.find_fn(h)
        while (pos := find(b"\n", pos + 1, eom)) != -1:
            count += 1
            ppos = pos
        if ppos + 1 < filesz:
            count += 1
        return count

    def readline(self, h: str, offset: int = 0) -> str:
        return self.k.file.readline(h, offset).decode("utf-8")

    # def read_lines(self, h: str, num: int, offset: int = 0) -> Iterator[str]:
    #     # CHECK? self._mm.seek(self._skipped_padding)
    #     self._mm.seek(offset - self._aligned_offset)
    #     readline_bytes = self._mm.readline
    #     # if not num:  # CASE: read till end or break from loop
    #     #     while lb := readline_bytes():
    #     #         yield lb.decode("utf-8")
    #     #     return
    #     for _ in range(num):
    #         if lb := readline_bytes():
    #             yield lb.decode("utf-8")
    #         else:
    #             break

    # ## UNUSED:PERF~: use memoryview() *only* when line length is > [1kB..64kB]
    # ##   <<WHY: it has 200 bytes overhead and per-byte indirection layer
    # # FUT:PERF? return byte-lines as-is and directly render them in curses as bytes ?
    # def read_lines_verylong(self, offset: int, num_lines: int = 100) -> list[str]:
    #     self._ensure_mmap()
    #     cur_pos = offset - self._aligned_offset
    #     mm = self._mm
    #     lines: list[str] = []
    #     assert mm
    #     find = self._mm.find
    #     with memoryview(mm) as mv:
    #         for _ in range(num_lines):
    #             # INFO: find() on mmap is fast; find() on memoryview is slow
    #             end_pos = find(b"\n", cur_pos)
    #             if end_pos == -1:
    #                 if cur_pos < len(mm):
    #                     lines.append(str(mv[cur_pos:], encoding="utf-8"))
    #                 break
    #             # INFO: abs_byteoffset = aligned_offset + cur_pos
    #             lines.append(str(mv[cur_pos:end_pos], encoding="utf-8"))
    #             cur_pos = end_pos + 1
    #     # FIXME: store byte extent [beg:end] -- to scroll-wnd-up too
    #     mm.seek(cur_pos)  # REM? update for later .readline
    #     return lines
