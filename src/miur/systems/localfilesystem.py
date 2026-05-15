import os
from typing import TYPE_CHECKING, Protocol

from .filecontent import FileContentProxy

if TYPE_CHECKING:

    class IKernel(Protocol):
        pass


class LocalFileSystem:
    __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel

    def listdir(self, h: str) -> list[str]:
        with os.scandir(h) as it:
            return [x.name for x in it]

    # BAD: where to cache mm/ino ? In OpenedFilesSystem to limit fd-open resources ?
    #   DECI: or combine with "scroll/view state" ?
    def file_content(self, h: str) -> FileContentProxy:
        return FileContentProxy(h)
