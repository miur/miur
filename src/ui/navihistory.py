from typing import Iterable, Iterator, override

from ..entity.base import Entity
from ..entity.error import ErrorEntry
from ..entity.fsentry import FSEntry
from ..util.logger import log
from .view import EntityView
from .vlst import SatelliteViewport


class EntityViewCachePool:
    def __init__(self) -> None:
        self._d: dict[Entity, EntityView] = {}

    def __iter__(self) -> Iterator[EntityView]:
        return iter(self._d.values())

    def get(self, ent: Entity) -> EntityView | None:
        return self._d.get(ent, None)

    # TODO: discard by frecency when we try to allocate more
    def add(self, ent: Entity) -> EntityView:
        # MAYBE:BET? reuse existing _wdg (from outside `*Layout) inof creating it per each entity
        #   OR:(vice versa): pass whole _view to any existing _wdg to display _lst
        view = EntityView(ent, wdgfactory=lambda: SatelliteViewport(self))
        self._d[ent] = view
        return view

    # USAGE: sys.getsizeof()
    @override
    def __sizeof__(self) -> int:
        raise NotImplementedError()


# CHG: store _view and _history in outside CacheDB and pass into NaviWidget only refs
#   >> then I can switch bw MillerNaviWidget and NPanelNaviWidget, preserving PWD/state/cache
#   TODO: single `History may have multiple `Cursors, each yielding different .focused_view
#     to their own instance of NaviWidget with shared cached pool of Entities
# SEP? "Cursor" vs "CacheDB"
# SEP? "load_all_parents_until" vs "history_switch_focus/trim"
# RENAME? {Navi,Loci}HistoryCursor
class HistoryCursor:
    def __init__(self, rootent: Entity, pool: EntityViewCachePool) -> None:
        ## NOTE: history is always prepopulated by known state (at least a RootNode)
        #   MAYBE: make RootNode into singleton ?
        #     ~~ BUT: I may need different "wf-restricted/focused" alt-views for RootNode
        # MAYBE:CHG: directly store "ent" (inof "view") in _stack to represent "xpath",
        #   as now we can use "_pool" -- to map it to temporarily cached "_view" when needed
        #   RENAME? _cursor_chain/navi_stack | _pool_cached_view/_view_pool
        view = pool.add(rootent)
        self._view_stack = [view]
        self._cursor_idx = 0
        # NOTE: tba to restore view when opening previously visited nodes
        self._pool = pool
        # DISABLED:FAIL: you need to hardcode .vh to make all nodes in ctor
        # self.jump_to(ent, intermediates=True)

    @property
    def pos(self) -> tuple[int, int]:
        return self._cursor_idx, len(self._view_stack)

    @override
    def __str__(self) -> str:
        # NOTE:FMT: "0/0" means we are at RootNode with no history
        # ALT:([]): use ⸤⸣ OR ⸢⸥
        hidx, hlen = self.pos
        return f"[{hidx}/{hlen-1}]"

    # FAIL: when split(horz) we will have *two* "current view" for same history
    #   MAYBE: store only "previous items" for history? but how to be with fwd-history ?
    @property
    def focused_view(self) -> EntityView:
        return self._view_stack[self._cursor_idx]

    def get_relative(self, off: int, /) -> EntityView | None:
        idx = off + self._cursor_idx
        if 0 <= idx < len(self._view_stack):
            return self._view_stack[idx]
        return None

    # RENAME? traverse_by(off=0)
    def go_back(self) -> None:
        if self._cursor_idx <= 0:
            # pylint:disable=protected-access
            log.trace("No back=" + self.focused_view._ent.name)
            return
        self._cursor_idx -= 1
        # NOTE: if we jumped over intermediates, we need to mark them, when they are shown in browse=
        self.focused_view._visited = True

    # BAD:ARCH:(part of distributed FSM): hard to track index permutations
    def _advance_or_retrieve_or_emplace(self, nent: Entity) -> int:
        # log.trace(f"{nent.loci=} | {self.pos}")  # <DEBUG
        log.info(f"{id(nent):x} {nent}")  # <DEBUG
        # NOTE: don't discard hist Entry on .go_back() to be able to return and see same EntityView
        #   CASE: advance inof discarding cached fs.commonpath() parents from left
        #     == verify path cmpts exist in expected positions of stack
        #     ALT~: .rfind() backwards until fs.commonpath(), then append as usual
        if (nview := self.get_relative(1)) and nview._ent == nent:
            return self._cursor_idx + 1
        # NOTE: retrieve cached View -- if we ever visited that Entry before
        # MOVE:RFC: use external shared `CachedEntities
        if not (v := self._pool.get(nent)):
            # REMOVE? do we need this fallback code to ensure EntityView was created ?
            #   ~ preview() is ought to always create `EntityView in _pool
            #     before we are able to "move into" that node
            # NOTE: we create a separate `SatelliteViewport per each `Entity assigned
            #   NICE: preserve "pos,vh,margin" as-is, and then reinterpret/resize only when going back
            #   ++ NICE: preserve the *generated* items as-is in the "_wdg._lst"
            #   +++ NICE: can use totally different *widgets* based on the type(_ent)
            #     e.g. Dashboard or Editor
            v = self._pool.add(nent)
            # HACK: clone current vlst vh/ww to newly created View
            # log.info(self._view_stack[self._cursor_idx]._wdg.sizehw)
            # CHECK: do I really need to do it? FAIL: initial node is also not resized
            #   DISABLED: on startup it clones sz=(0,0), which is useless, and then we got proper resize()
            # v._wdg.resize(*self._view_stack[self._cursor_idx]._wdg.sizehw)

        # log.trace(f"{v._ent.loci=} | {self.pos}")  # <DEBUG
        ## SPLIT?: _append_or_replace(v)
        # NOTE: discard the rest of navi stack if we go into different route (but preserve in _pool)
        self._view_stack[self._cursor_idx + 1 :] = [v]
        return len(self._view_stack) - 1
        # log.trace(f"{self._view_stack} | {self.pos}")  # <DEBUG

    # NOTE: can jump to distant/unrelated node in one step
    #   BAD!※⡧⢄⠋⡎ "jump_to(unrelated path)" is discarded from history after back-n-forth
    # BET?RENAME?(jump_to): restore_or_load(ent)
    # RENAME?(:intermediates): {remember,cache,traverse}_parents
    def jump_to(self, nent: Entity, intermediates: bool = False) -> None:
        # TEMP:CHG: use true NonExplorableAtomic() to prevent recursive exploration
        if isinstance(nent, ErrorEntry) and nent.name == "Atomic(N/A)":
            return

        # pylint:disable=protected-access
        # if self.focused_view._ent == nent:
        #     log.warning(f"Same: {nent=}")
        #     return
        assert self.focused_view._ent != nent

        # log.trace(list(nent.parents_loci()))
        # log.trace(f"{nent}{self.pos}")  # <DEBUG

        # ALT:THINK:SPLIT: `NaviModel which knows when to yeild `RootNode (which holds rootfs/stdin/etc providers)
        if intermediates:
            if not isinstance(nent, FSEntry):
                raise NotImplementedError(type(nent))
            # vh_fallback = self.focused_view._wdg._viewport_height_lines
            # FIXME: start from nearest common parent inof always from RootNode
            self._cursor_idx = 0
            for ploci in nent.parents_loci():
                # NOTE: adjust cursor to hover over entity you came back from
                #   !! should fixup existing entities in history._stack too
                # BAD:PERF: we are forced to traverse each intermediate entity
                #   to use the *same* instances of FSEntry as returned by .explore()
                if wi := self.focused_view._wdg.focus_on(
                    lambda w, plo=ploci: w._ent.loci == plo
                ):
                    self._cursor_idx = self._advance_or_retrieve_or_emplace(wi._ent)
                else:
                    raise ValueError(f"WTF: intermediate node doesn't exist: {ploci=}")

        if not (wi := self.focused_view._wdg.focus_on(nent.name)):
            raise ValueError(
                f"TEMP:DECI: current view={self.focused_view} has no nm={nent.name}"
            )
        # WARN: {id(wi._ent) != id(nent)}
        #  COS: "nent" is an arbitrary `Entity, and "wi._ent" is from a contiguous graph
        self._cursor_idx = self._advance_or_retrieve_or_emplace(wi._ent)
        # NOTE: we skip intermediates, COS they weren't shown in browse= yet
        self.focused_view._visited = True
        # log.trace(f"{nent}{self.pos}")  # <DEBUG
        # log.trace(self._view_stack)  # <DEBUG
        # log.trace(list(self._pool))  # <DEBUG

    ## FAIL: `Entry.parent() is not generalizable ※⡧⢃⠬⢖
    ## BET: traverse and preload all intermediate parents on __init__(path)
    ##   NICE: for untraversables (e.g. "mpd://song") we always have preloaded RootNode as "prev"
    ##     BAD!※⡧⢄⠋⡎ "jump_to(unrelated path)" is discarded from history after back-n-forth
    # def go_parent(self):
    #     # pylint:disable=protected-access
    #     if not isinstance(self._view._ent, FSEntry):
    #         raise NotImplementedError()
    #     pview = self._view
    #     # RND:(controversial): as we basically navigate to *new* nodes to the left,
    #     #   so we should keep the history of this navigation, but we discard that
    #     p = self._view._ent._x
    #     pp = fs.dirname(p)
    #     if pp != p:
    #         parent_ent = FSEntry(pp)
    #         self._view = EntityView(parent_ent)  # , hint_idx=0)
    #         self._view_stack = [self._view]
    #         self._cursor_idx = len(self._view_stack) - 1
    #         # NOTE: set cursor onto entity you came back from
    #         for i, e in enumerate(self._view._wdg._lst):
    #             if e._ent.name == pview._ent.name:
    #                 self._view._wdg._viewport_followeditem_lstindex = i
    #                 self._view._wdg._cursor_item_lstindex = i
    #                 # BAD: hardcoding pos to avoid last item at top
    #                 pos = pview._wdg._viewport_height_lines // 2
    #                 self._view._wdg._viewport_followeditem_linesfromtop = pos
    #                 break
    #         if self._view._wdg._cursor_item_lstindex != i:
    #             log.error("WTF: pview not found")

    def dump(self) -> str:
        # NOTE: also preserve/restore current cursor
        cursnm = self.focused_view._wdg.focused_item._ent.name
        hidx = str(self._cursor_idx)
        entnms = (v._ent.name for v in self._view_stack)
        return "".join(f"\0{s}\n" for s in [cursnm, hidx, *entnms])

    def load(self, text: str) -> None:
        it = iter(("\n" + text + "\0").split("\n\0"))
        empty = next(it)
        assert empty == ""
        cursnm = next(it)
        hidx = int(next(it))
        self._cursor_idx = 0
        rootnm = next(it)
        assert rootnm == self.focused_view._ent.name
        for nm in it:
            if not nm:
                continue
            # BET? store both "idx" and "nm" to have fallback for focus_on() ?
            if not (wi := self.focused_view._wdg.focus_on(nm)):
                # ALG: load as much as possible, and only ntf/log the error
                #   COS: old directory may disappear OR explored error may become fixed
                msg = f"{self!s} not found {nm=} under view={self.focused_view}"
                log.error(msg)
                # raise ValueError(msg)
                return  # <COS: hidx/cursnm lost their meaning
            self._cursor_idx = self._advance_or_retrieve_or_emplace(wi._ent)
        self._cursor_idx = hidx
        self.focused_view._visited = True
        self.focused_view._wdg.focus_on(cursnm)
