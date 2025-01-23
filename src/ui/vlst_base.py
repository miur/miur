from functools import lru_cache
from typing import TYPE_CHECKING, Protocol, Sequence

from .item import ItemWidget

if TYPE_CHECKING:
    # FIXED: circular import
    #   ALT:BET?(C++-style): use pure `PoolProtocol (w/o IMPL)
    from .navihistory import EntityViewCachePool


class SatelliteViewport_DataProtocol(Protocol):
    _lst: Sequence[ItemWidget]
    _pool: "EntityViewCachePool"
    _viewport_followeditem_lstindex: int
    _viewport_followeditem_linesfromtop: int
    _viewport_origin_yx: tuple[int, int]
    _viewport_height_lines: int
    _viewport_width_columns: int
    _viewport_margin_lines: int
    _cursor_item_lstindex: int
    _item_maxheight_hint: int

    @lru_cache
    def _fih(self, i: int) -> int:
        # IDEA:OPT: scale maxlines with viewport height, i.e. use smaller preview for smaller windows
        # BAD: should account for indents inside viewport {wrapw = vw - 2 - indent; assert iw > 4}
        wrapw = self._viewport_width_columns
        maxln = self._item_maxheight_hint
        lines = self._lst[i].struct(wrapwidth=wrapw, maxlines=maxln)
        assert lines
        return len(lines)
