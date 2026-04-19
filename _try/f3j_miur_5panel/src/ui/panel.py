from typing import Iterable, Iterator, Self, override

from ..alg.flowratio import flowratio_to_abs
from .rect import Rect


class Panel:
    _rect: Rect

    # RENAME?(space_alloci):COS:BAD:(ratio): contains fixed/flexed panel size, inof rational fraction
    def __init__(
        self,
        name: str = "",
        split: list[Self] | None = None,
        *,
        w: int | float = 0,
        h: int | float = 0,
        sepw: int = 0,
        # visible: bool = True,
    ) -> None:
        self.name = name
        self._hint_wh = (w, h)
        # self.size = size
        self._split = split if split is not None else []
        # self._visible = visible  # RENAME? enabled,present
        self._sepw = sepw

    @property
    def rect(self) -> Rect:
        return self._rect

    @override
    def __repr__(self) -> str:
        return f"{self.name}{self._rect}:({", ".join(map(repr,self._split))})"

    def __len__(self) -> int:
        return len(self._split)

    def __iter__(self) -> Iterator[Self]:
        return iter(self._split)

    def __getitem__(self, nm: str) -> Self | None:
        if nm == self.name:
            return self
        return next((p for p in self._split if p[nm] is not None), None)

    # def __contains__(self, nm: str) -> bool:
    #     if nm == self.name:
    #         return True
    #     return any(nm in p for p in self._split)

    def named_rects(self) -> Iterable[tuple[str, Rect]]:
        for p in self._split:
            if nm := p.name:
                yield (nm, p._rect)
            yield from p.named_rects()

    def sep_rects(self) -> Iterable[tuple[str, Rect]]:
        assert hasattr(self, "_rect"), "Err: call .resize() beforehand"
        pr: Rect | None = None
        for p in self._split:
            # NOTE: go from left to right
            yield from p.sep_rects()
            r = p.rect
            if pr:
                yield (p.name, Rect(w=r.x - pr.xw, h=pr.h, x=pr.xw, y=pr.y))
            pr = r

    def resize(self, maxrect: Rect) -> None:
        # NOTE: _hint_wh is only used by its parent and otherwise ignored
        self._rect = maxrect
        if not self._split:
            return
        # TODO? incorporate .visible=0/1 to affect flexed area
        # RND:TEMP: hardcoded=VSplit; keep vh=0 to stretch to full Rect.height
        assert all(x._hint_wh[1] == 0 for x in self._split)
        ratio = [x._hint_wh[0] for x in self._split]
        allsepw = self._sepw * (len(ratio) - 1)
        ws = flowratio_to_abs(ratio, maxrect.w - allsepw)
        vx = maxrect.x
        for p, w in zip(self._split, ws):
            p.resize(Rect(w, maxrect.h, x=vx, y=maxrect.y))
            # INFO: we don't need spacer column after rightmost vlst
            #   >> it should be a .frame or a .margin
            vx += w + self._sepw
