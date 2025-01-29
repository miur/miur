from typing import Iterable, override

from .base import Golden


# class ErrorEntry(HaltEntry(Atomic))
class ErrorEntry(Golden):
    def __init__(self, msg: str, loci: tuple[str, ...] | None = None) -> None:
        self._msg = msg
        self._orig = loci

    @override
    @property
    def name(self) -> str:
        return self._msg

    @override
    @property
    def loci(self) -> str:
        return "".join(self._orig) if self._orig else "∅ " + repr(self)

    @override
    def explore(self) -> Iterable[Golden]:
        raise NotImplementedError(self._msg)
