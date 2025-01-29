from typing import Iterable, override

from .base import Golden
from .error import ErrorEntry


class TextEntry(Golden):
    __slots__ = ()  # "name", "loci", "explore")

    def __init__(self, text: str, loci: tuple[str, ...] | None = None) -> None:
        self._x = text
        self._at = loci  # = (path,lnum,[col,[boff,...]])

    @override
    @property
    def name(self) -> str:
        return self._x

    @override
    @property
    def loci(self) -> str:
        return "".join(self._at) if self._at else "∅ " + repr(self)

    @override
    def explore(self) -> Iterable[Golden]:
        from re import finditer

        cls = type(self)
        words = [
            cls(
                m.group(0),
                loci=(
                    (
                        ## DISABLED: interferes with !nvim jumping to line under cursor
                        # self._at[0],
                        # f":{m.start()}+{m.end() - m.start()}",
                        # self._at[-1],
                        *self._at,
                        f":{m.start()+1}",
                    )
                    if self._at
                    else ("∅", f":{m.start()}+{m.end() - m.start()}")
                ),
            )
            for m in finditer(r"\S+", self._x)
        ]
        if len(words) == 1:
            return [ErrorEntry("INDIVISIBLE WORD", loci=self._at)]
        return words
