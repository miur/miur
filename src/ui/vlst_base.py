from typing import Protocol, Sequence

from .entity_base import Representable


class SatelliteViewport_DataProtocol(Protocol):
    _lst: Sequence[Representable]
    _viewport_followeditem_lstindex: int
    _viewport_followeditem_linesfromtop: int
    _viewport_origin_yx: tuple[int, int]
    _viewport_height_lines: int
    _viewport_width_columns: int
    _viewport_margin_lines: int
    _cursor_item_lstindex: int

    # MAYBE: make ItemWidget to calc() item height and draw it (or only ItemXfm)
    #   WARN: don't store "index" inside ItemWidget << PERF:(slow): rebuild on each order-by
    #     ~~ though, I can pass list index ctx into ItemWidget.render(ctx[index=i])
    #   NICE:IDEA: make scrollbar ~imprecise/elastic~ and scroll through *items* when jumping far,
    #     but scroll by lines when scrolling around current viewport, where we already know all items heights
    @staticmethod
    def _itemheight(item: Representable) -> int:
        return item.name.count("\n") + 1


# pylint:disable=too-many-instance-attributes
class SatelliteViewportBase(SatelliteViewport_DataProtocol):
    def __init__(self) -> None:
        self._lst: Sequence[Representable]
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

    # ARCH: when we have multiple cursors "focused_item" is the item under currently active cursor
    #    MAYBE:THINK: use .subfocus(canvas_line/word) to apply actions to specific auxinfo of focused item
    @property
    def focused_item(self) -> Representable:
        # MAYBE:XLR: point cursor to folder/_ent itself
        #   + makes cursor always deterministic
        #   + allows you the subset of operations, like adding new files to the folder
        if not self._lst:
            # BET? return dummy placeholder for empty dirs
            #   BAD! placeholder is *content*, it shouldn't be *focused* either
            #   ALT: always include dir itself in listing -- so we could do ops from inside the dir
            raise IndexError("empty list")
        return self._lst[self._cursor_item_lstindex]

    def resize(self, vh: int, vw: int, origin: tuple[int, int] = (0, 0)) -> None:
        pvh = self._viewport_height_lines
        self._viewport_origin_yx = origin
        self._viewport_height_lines = vh
        self._viewport_width_columns = vw
        self._viewport_margin_lines = vh // 8  # OR: fixed=2
        # KEEP: self._viewport_followeditem_lstindex
        # RND: adjust resulting offset to align onto margin
        #   ALT:BAD: self._viewport_followeditem_linesfromtop = 0
        if pvh > 0 and (ratio := self._viewport_followeditem_linesfromtop / pvh) > 0:
            self._viewport_followeditem_linesfromtop = int(vh * ratio)

    # CASE:(lightweight): to be able to re-assign ~same list after external xfm, e.g. after "order-by"
    def assign(self, lst: Sequence[Representable], hint_idx: int | None = None) -> None:
        pidx = self._cursor_item_lstindex if hint_idx is None else hint_idx
        focused = self._lst[pidx] if getattr(self, "_lst", None) else None
        # WARN: whole function should be atomic
        #   i.e. "cursor,canvas" should always be in boundaries of "lst"
        # TODO: pre-load only visible part fitting into viewport
        #   WARN: on first assign(), viewport height may still be =0, due to -resize() being called later
        self._lst = lst
        newidx = self._reindex(pidx, focused)
        self._viewport_followeditem_lstindex = self._cursor_item_lstindex = newidx
        ## DISABLED: we always keep the previous position of cursor on the screen
        #   self._viewport_followeditem_linesfromtop = 0

    def _reindex(self, pidx: int, focused: Representable | None) -> int:
        # NOTE: keep cursor on same item::
        #   * if any items were inserted/deleted before idx
        #   * if "order-by" have changed its item's idx in _lst
        if focused is None:
            return 0
        if pidx < len(self._lst) and focused is self._lst[pidx]:
            return pidx
        try:
            return self._lst.index(focused)
        except ValueError:
            ## TEMP: reset *cursor* position on .assign(newlst)
            # TODO: if item under cursor had disappeared we can temp-reinsert the _focused_item into the list
            #   and seek for it to find a new index, then pick item before or after expected position
            return 0
