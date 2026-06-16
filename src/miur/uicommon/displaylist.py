from typing import Iterable, NamedTuple


class TextSpan(NamedTuple):  # RENAME: CellSpan
    x: int
    y: int
    t: str
    wc: int  # <MAYBE? cache cell-width hint for renderer's BoundingBox
    aid: int  # WARN: don't use DFL=0 here -- always set style explicitly to .default
    zi: int = 1  # < Z-index of whole layer bucket (NOT per-element)
    ## ALT:
    # @property
    # def wc(self) -> int:
    #     return width(self.t)


type DisplayList = list[TextSpan]
type DisplayStream = Iterable[TextSpan]
