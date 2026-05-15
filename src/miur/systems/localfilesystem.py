import os
from typing import TYPE_CHECKING, Protocol

if TYPE_CHECKING:

    class IKernel(Protocol):
        pass


type FDInt = int
type HPath = str


class LocalFileSystem:
    __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel

    def listdir(self, h: HPath) -> list[str]:
        with os.scandir(h) as it:
            return [x.name for x in it]
