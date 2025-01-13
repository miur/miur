from functools import lru_cache
from typing import Callable, Protocol, Sequence

from ..util.logger import log
from .entity_base import Golden
from .item import ItemWidget


class SatelliteViewport_DataProtocol(Protocol):
    _lst: Sequence[ItemWidget]
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
        return len(self._lst[i].struct(wrapwidth=wrapw, maxlines=maxln))


# pylint:disable=too-many-instance-attributes
class SatelliteViewportBase(SatelliteViewport_DataProtocol):
    def __init__(self) -> None:
        # NOTE: actually _lst here stands for a generic _augdbpxy with read.API
        #   i.e. DB augmented by virtual entries, all generated-and-cleared on demand
        self._lst: Sequence[ItemWidget]
        # ARCH:
        #  * when "viewport follows cursor", then followeditem==item_under_cursor,
        #    with offset being the same as for cursor itself
        #    >> NICE: even if items around cursor will be removed/inserted/changed,
        #      viewport will stay in same place relative to item under cursor
        #  * when "viewport freely scrolls", then followeditem==top/bot item in viewport,
        #    based on direction of the current scroll, with offset sticking to that item
        self._viewport_followeditem_lstindex = 0
        # NOTE: will become negative *only* when scrolling past first line of last multiline item
        self._viewport_followeditem_linesfromtop = 0
        self._viewport_origin_yx = (0, 0)
        self._viewport_height_lines = 0
        self._viewport_width_columns = 0  # <RQ: for right-justified table items
        # WARN:(margin): should be counted in "lines" inof "items"
        #   !! orse margin over several large multiline items may even push cursor out of the viewport
        self._viewport_margin_lines = 0
        self._cursor_item_lstindex = 0
        self._item_maxheight_hint = 0

    # ARCH: when we have multiple cursors "focused_item" is the item under currently active cursor
    #    MAYBE:THINK: use .subfocus(canvas_line/word) to apply actions to specific auxinfo of focused item
    @property
    def focused_item(self) -> ItemWidget:
        # MAYBE:XLR: point cursor to folder/_ent itself
        #   + makes cursor always deterministic
        #   + allows you the subset of operations, like adding new files to the folder
        if not self._lst:
            # BET? return dummy placeholder for empty dirs
            #   BAD! placeholder is *content*, it shouldn't be *focused* either
            #   ALT: always include dir itself in listing -- so we could do ops from inside the dir
            raise IndexError("empty list")
        return self._lst[self._cursor_item_lstindex]

    @property
    def sizehw(self) -> tuple[int, int]:
        return (self._viewport_height_lines, self._viewport_width_columns)

    # RENAME: set_viewport(vw, vh, vy, vx)
    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        pvh = self._viewport_height_lines
        self._viewport_origin_yx = origin
        self._viewport_height_lines = vh
        self._viewport_width_columns = vw
        self._viewport_margin_lines = vh // 8  # OR: fixed=2
        self._item_maxheight_hint = 1 + (vh // 10)
        # KEEP: self._viewport_followeditem_lstindex
        # RND: adjust resulting offset to align onto margin
        #   ALT:BAD: self._viewport_followeditem_linesfromtop = 0
        if pvh > 0 and (ratio := self._viewport_followeditem_linesfromtop / pvh) > 0:
            self._viewport_followeditem_linesfromtop = int(vh * ratio)

    ## IDEA: if not found -- repeat search once again for _orig_lst,
    #     but set cursor on the first visible item (or completely hide cursor)
    #   NICE: should work even if this entry was hidden/filtered from parent.xfm_lst
    def focus_on(
        self,
        key: int | float | ItemWidget | Golden | str | Callable[[ItemWidget], bool],
        # HACK:(vh_fallback): is needed by history.jump_to() before .resize() call
        #   BET:(init): call .resize() and then explicitly .jump_to(intermediates=True)
        #   ALT:API: pos_hint=
        vh_fallback: int = 0,
    ) -> ItemWidget | None:
        if not self._lst:
            raise NotImplementedError("DECI:WiP")

        vh = self._viewport_height_lines or vh_fallback
        assert 0 < vh < 100

        def _match(cond: Callable[[ItemWidget], bool]) -> int | None:
            return next((i for i, w in enumerate(self._lst) if cond(w)), None)

        idx: int | None
        match key:
            case int():
                idx = key
            case float():  # ALT:USAGE: float(fractions.Fraction('3/7'))
                idx = round(vh * key)
            case ItemWidget():
                try:
                    idx = self._lst.index(key)
                except ValueError:
                    idx = None
            case Golden():
                # pylint:disable=protected-access
                idx = _match(lambda w: w._ent == key)
            case str():
                # pylint:disable=protected-access
                # TEMP:BAD: it's a mess to cmp either .name or .loci (NEED: strict FMT)
                idx = _match(lambda w: w._ent.loci == key)
            case cond if callable(cond):
                idx = _match(cond)
            case _:
                raise TypeError("Unsupported type")

        if idx is None:
            return None
        if idx < 0:
            idx += len(self._lst)
        if idx < 0 or idx >= len(self._lst):
            raise ValueError((idx, key))
        self._viewport_followeditem_lstindex = self._cursor_item_lstindex = idx

        # NOTE: recalc anticipated "pos" for the focused item
        #   ENH? inof DFL=vh//2 tiac .margin and direction of last step_by()
        #   BET?ALT:(reuse "step_by" IMPL):FAIL: it steps by "steps" inof "items"
        #     self.step_by((len(self._lst) + idx if idx < 0 else idx) - self._cursor_item_lstindex)
        pos = vh // 2
        if idx < pos and (top := sum(self._fih(i) for i in range(0, idx))) < pos:
            pos = top
        elif (
            idx > len(self._lst) - pos
            and (bot := sum(self._fih(i) for i in range(idx, len(self._lst) - 1))) < pos
        ):
            pos = vh - self._fih(len(self._lst) - 1) - bot
        self._viewport_followeditem_linesfromtop = pos
        log.warning(pos)

        return self._lst[idx]

    # CASE:(lightweight): to be able to re-assign ~same list after external xfm, e.g. after "order-by"
    def assign(self, lst: Sequence[Golden], hint_idx: int | None = None) -> None:
        pidx = self._cursor_item_lstindex if hint_idx is None else hint_idx
        focused = self._lst[pidx] if getattr(self, "_lst", None) else None
        # BAD: can't drop individual items REF⌇⡧⡺⣽⡠
        #   https://stackoverflow.com/questions/56413413/lru-cache-is-it-possible-to-clear-only-a-specific-call-from-the-cache
        self._fih.cache_clear()
        # WARN: whole function should be atomic
        #   i.e. "cursor,canvas" should always be in boundaries of "lst"
        # TODO: pre-load only visible part fitting into viewport
        #   WARN: on first assign(), viewport height may still be =0, due to -resize() being called later
        # FUT:PERF: don't instantiate all `ItemWidgets for _lst at once ※⡧⡺⣩⠺
        self._lst = [ItemWidget(x) for x in lst]
        newidx = self._reindex(pidx, focused)
        self._viewport_followeditem_lstindex = self._cursor_item_lstindex = newidx
        ## DISABLED: we always keep the previous position of cursor on the screen
        #   self._viewport_followeditem_linesfromtop = 0

    def _reindex(self, pidx: int, focused: ItemWidget | None) -> int:
        # NOTE: keep cursor on same item::
        #   * if any items were inserted/deleted before idx
        #   * if "order-by" have changed its item's idx in _lst
        if focused is None:
            return 0
        if pidx < len(self._lst) and focused is self._lst[pidx]:
            return pidx
        try:
            # FIXME: should search by constant `Entity inof volatile `ItemWidget
            return self._lst.index(focused)
        except ValueError:
            ## TEMP: reset *cursor* position on .assign(newlst)
            # TODO: if item under cursor had disappeared we can temp-reinsert the _focused_item into the list
            #   and seek for it to find a new index, then pick item before or after expected position
            return 0
