import os
from dataclasses import dataclass
from typing import TYPE_CHECKING, Protocol

if TYPE_CHECKING:
    from .localfilesystem import LocalFileSystem
    from .textsystem import TextSystem

    class IKernel(Protocol):
        lfs: LocalFileSystem
        text: TextSystem


@dataclass(slots=True, kw_only=True)
class Item:
    text: str
    idx: int
    h: object


class ViewSystem:
    __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel

    # FIXME: should be able to return Infinity or Unknown
    def len_items(self, h: str) -> int:
        if os.path.isdir(h):
            return len(self.k.lfs.listdir(h))
        return self.k.text.count_lines(h)

    # TODO:OPT: props-dict to enrich item from other systems
    def get_item(self, h: str, i: int) -> Item:
        # if self.order_by:
        #     self._lst.sort(key=self.order_by)  # OR key=str
        if os.path.isdir(h):
            dlst = self.k.lfs.listdir(h)
            nm = dlst[i]
            return Item(text=nm, idx=i, h=nm)
        # FIXME: cache loff to continue seeking next line
        #   OR:BET? new func to iterate from starting line
        loff = self.k.text.seek_fwd_to_line_nth(h, i)
        line = self.k.text.readline(h, offset=loff)
        return Item(text=line, idx=i, h=(loff, line))
