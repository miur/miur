from typing import TYPE_CHECKING, Protocol

if TYPE_CHECKING:

    class IKernel(Protocol):
        pass


class TextSystem:
    __slots__ = ("k",)

    def __init__(self, kernel: IKernel) -> None:
        self.k = kernel
