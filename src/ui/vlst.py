from typing import TYPE_CHECKING, Callable, Sequence

from ..util.logger import log
from .entity_base import Golden
from .item import ItemWidget
from .vlst_base import SatelliteViewport_DataProtocol
from .vlst_redraw import SatelliteViewport_RedrawMixin
from .vlst_stepby import SatelliteViewport_StepbyMixin

if TYPE_CHECKING:
    # FIXED: circular import
    #   ALT:BET?(C++-style): use pure `PoolProtocol (w/o IMPL)
    from .navihistory import EntityViewCachePool


# pylint:disable=too-many-instance-attributes
class SatelliteViewport(
    # REF: https://stackoverflow.com/questions/10018757/how-does-the-order-of-mixins-affect-the-derived-class
    #   (order): same as "class Viewport(Stepby(Redraw(Base))"
    # BUG:(mypy): base protocol can't be placed last (after mix-ins, as it's supposed to be)
    #  | Cannot determine type of "_viewport_*" in base class "SatelliteViewport_StepbyMixin"
    SatelliteViewport_DataProtocol,
    SatelliteViewport_StepbyMixin,
    SatelliteViewport_RedrawMixin,
):
    # NOTE: actually _lst here stands for a generic _augdbpxy with read.API
    #   i.e. DB augmented by virtual entries, all generated-and-cleared on demand
    _lst: Sequence[ItemWidget]

    ## XP~HACK: split class over several files w/o mix-ins (inof complex inheriting)
    ## SRC: https://stackoverflow.com/questions/39740632/python-type-hinting-without-cyclic-imports
    ## INFO: import global mix-in methods directly into class
    # from .vlst_stepby import SatelliteViewport_StepbyMixin.step_by

    def __init__(self, pool: "EntityViewCachePool") -> None:
        self._pool = pool  # TEMP: only for render(spacermark)
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
        # HACK: for correct .pos trigger centering on very first .resize()
        pos = self._viewport_followeditem_linesfromtop
        if pvh == 0 and pos == 0:
            self.center_viewport_on_cursor()
        # RND: adjust resulting offset to align onto margin
        #   ALT:BAD: self._viewport_followeditem_linesfromtop = 0
        elif pvh > 0 and (ratio := pos / pvh) > 0:
            # BAD: rounding of long lists may jump by +/-3  e.g. 33 -> 36
            self._viewport_followeditem_linesfromtop = int(vh * ratio)

    ## IDEA: if not found -- repeat search once again for _orig_lst,
    #     but set cursor on the first visible item (or completely hide cursor)
    #   NICE: should work even if this entry was hidden/filtered from parent.xfm_lst
    # [_] FAIL:ALSO: we can't set subcursor w/o proper ._item_maxheight_hint derived from vh
    #   => therefore we still need "vh_fallback" to focus_on()
    #   ALSO:(key=vpidx): focus on item visible at viewport fraction or at Nth position
    def focus_on(
        self,
        key: int | float | ItemWidget | Golden | str | Callable[[ItemWidget], bool],
        # HACK:(vh_fallback): is needed by history.jump_to() before .resize() call
        #   BET:(init): call .resize() and then explicitly .jump_to(intermediates=True)
        #   ALT:API: pos_hint=
        # vh_fallback: int = 0,
    ) -> ItemWidget | None:
        # NOTE: "ListIsEmpty" should behave the same as "ItemNotFound"
        #   COS: you can't find *any* item inside of empty list, and it's fine
        #   WF: .focus_on() is used in many direct and derived keybinds,
        #     which expect to silently ignore the error OR to notify of needle absence.
        #   INFO: by same logic there is no sense to raise exception from .focus_on(),
        #     only to be forced to intercept it in *each* keybind-related API
        if not self._lst:
            log.trace("ListIsEmpty <- .focus_on " + str(key))
            return None  # MAYBE? return ErrorEntity("ListIsEmpty")

        def _match(cond: Callable[[ItemWidget], bool]) -> int | None:
            return next((i for i, w in enumerate(self._lst) if cond(w)), None)

        idx: int | None
        match key:
            case int():
                # ALSO:(key=vpidx): focus on Nth item visible in viewport
                idx = key
            case float():  # ALT:USAGE: float(fractions.Fraction('3/7'))
                # ALSO:(key=vpidx): focus on item visible at viewport fraction
                # idx = round(vh * key)
                assert 0.0 <= key <= 1.0
                idx = round(len(self._lst) * key)
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
                #   [_] BET:TODO: use "str" solely for .name and use custom `Loci() for composite .loci ※⡧⢈⢲⠜
                idx = _match(lambda w: w._ent.loci == key)
            case cond if callable(cond):
                idx = _match(cond)
            case _:
                raise TypeError("Unsupported type")

        if idx is None:
            log.trace("ItemNotFound <- .focus_on " + str(key))
            return None  # MAYBE? return ErrorEntity("ItemNotFound")
        if idx < 0:
            idx += len(self._lst)
        if idx < 0 or idx >= len(self._lst):
            raise ValueError((idx, key))
        self._viewport_followeditem_lstindex = self._cursor_item_lstindex = idx
        log.warning(f"{idx=} <- .focus_on {key}")
        # RND: always center cursor on .focus_on() to see the most surroundings of cursor
        if self._viewport_height_lines > 0:
            self.center_viewport_on_cursor()
        return self._lst[idx]

    def center_viewport_on_cursor(self) -> None:
        # NOTE: recalc anticipated "pos" for the focused item
        #   ENH? inof DFL=vh//2 tiac .margin and direction of last step_by()
        #   BET?ALT:(reuse "step_by" IMPL):FAIL: it steps by "steps" inof "items"
        #     self.step_by((len(self._lst) + idx if idx < 0 else idx) - self._cursor_item_lstindex)
        vh = self._viewport_height_lines
        assert 0 < vh < 100
        idx = self._viewport_followeditem_lstindex
        pos = vh // 2
        if idx < pos and (top := sum(self._fih(i) for i in range(0, idx))) < pos:
            pos = top
        elif (
            idx > len(self._lst) - pos
            and (bot := sum(self._fih(i) for i in range(idx, len(self._lst) - 1))) < pos
        ):
            # BUG: snaps vlst to bot
            # BET: use step_by(0) to refresh .pos and reuse DFL:ALG
            pos = vh - self._fih(len(self._lst) - 1) - bot
        # log.warning(f"{pos=} vs {vh//2=} <- .center_cursor")  # <DEBUG
        self._viewport_followeditem_linesfromtop = pos

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
