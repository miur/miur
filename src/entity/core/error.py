from typing import override

from ..base.golden import Entities, Entity, Golden, StopExploration
from .text import TextEntry


# class ErrorEntry(HaltEntry(Atomic))
class ErrorEntry(Golden[str]):
    # def __init__(self, msg: str, loci: tuple[str, ...] | None = None) -> None:
    def __init__(
        self, *, parent: Entity, name: str | None = None, exc: Exception | None = None
    ) -> None:
        nm = name if name else exc.__class__.__name__ if exc else "ERROR"
        self._exc = exc
        super().__init__(nm, parent)
        # self._orig = loci

    # @override
    # @property
    # def name(self) -> str:
    #     return self._msg

    # @override
    # @property
    # def loci(self) -> str:
    #     return "".join(self._orig) if self._orig else "∅ " + repr(self)

    @override
    def explore(self) -> Entities:
        # CHG: make it explorable -- especially in flattened dir
        #   * show originator `Entity and `Action which resulted in ERR
        #   * show the exception and its backgrace which resulted in this error
        if self._exc is None:
            raise StopExploration()

        from traceback import format_exception

        return [
            TextEntry(l.rstrip("\n"), self)
            for l in format_exception(self._exc, chain=True)
        ]
