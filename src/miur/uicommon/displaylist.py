from collections.abc import Iterable
from typing import NamedTuple

from .styleids import StyleId


class TextSpan(NamedTuple):  # RENAME: CellSpan
    x: int
    y: int
    t: str
    wc: int  # <MAYBE? cache cell-width hint for renderer's BoundingBox
    aid: StyleId  # ATT: always ctor to Aid.DEFAULT inof using .aid=0 here
    zi: int = 1  # < Z-index of whole layer bucket (NOT per-element)
    ## ALT:
    # @property
    # def wc(self) -> int:
    #     return width(self.t)


type DisplayList = list[TextSpan]
type DisplayStream = Iterable[TextSpan]
