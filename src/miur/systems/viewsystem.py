from dataclasses import dataclass
from typing import TYPE_CHECKING, Protocol

if TYPE_CHECKING:
    from .localfilesystem import LocalFileSystem

    class IKernel(Protocol):
        lfs: LocalFileSystem


@dataclass(slots=True, kw_only=True)
class Item:
    text: str
    idx: int
    h: object


class ViewSystem:
    __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel

    def len_items(self, h: str) -> int:
        # FIXME: should be able to return Infinity or Unknown
        return self.k.lfs.file_content(h).count_lines()

    def get_item(self, h: str, i: int) -> Item:
        # if self.order_by:
        #     self._lst.sort(key=self.order_by)  # OR key=str
        # FIXME: cache loff to continue seeking next line
        #   OR:BET? new func to iterate from starting line
        proxy = self.k.lfs.file_content(h)
        loff = proxy.seek_fwd_to_line_nth(i)
        line = next(proxy.read_lines(1, offset=loff))
        return Item(text=line, idx=i, h=(loff, line))
