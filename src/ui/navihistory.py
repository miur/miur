import os.path as fs

from ..util.logger import log
from .entity_base import Golden
from .entries import ErrorEntry, FSEntry, RootEntry
from .view import EntityView


# CHG: store _view and _history in outside CacheDB and pass into NaviWidget only refs
#   >> then I can switch bw MillerNaviWidget and NPanelNaviWidget, preserving PWD/state/cache
#   TODO: single `History may have multiple `Cursors, each yielding different .focused_view
#     to their own instance of NaviWidget with shared cached pool of Entities
# SEP? "Cursor" vs "CacheDB"
# SEP? "load_all_parents_until" vs "history_switch_focus/trim"
class HistoryCursor:
    def __init__(self, ent: Golden) -> None:
        ## NOTE: history is always prepopulated by known state (at least a Root Node)
        #   MAYBE: make RootEntry into singleton ?
        #     ~~ BUT: I may need different "wf-restricted/focused" alt-views for RootEntry
        # MAYBE:CHG: directly store "ent" in _stack to represent "xpath",
        #   as now we can use "_pool" -- to map it to temporarily cached "_view"
        #   RENAME? _cursor_chain/navi_stack | _pool_cached_view/_view_pool
        self._view_stack = [EntityView(RootEntry())]
        self._cursor_idx = len(self._view_stack) - 1
        # pylint:disable=protected-access
        # NOTE: tba to restore view when opening previously visited nodes
        self._cache_pool = {x._ent: x for x in self._view_stack}
        if ent != self.focused_view._ent:
            self.jump_to(ent, intermediates=True)

    @property
    def pos(self) -> tuple[int, int]:
        return self._cursor_idx, len(self._view_stack)

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
            log.trace(self.focused_view._ent.name)
            return
        self._cursor_idx -= 1

    def _advance_or_retrieve_or_emplace(self, nent: Golden) -> None:
        # NOTE: don't discard hist Entry on .go_back() to be able to return and see same EntityView
        if (nview := self.get_relative(1)) and nview._ent == nent:
            self._cursor_idx += 1
            return
        # NOTE: retrieve cached View -- if we ever visited that Entry before
        if not (v := self._cache_pool.get(nent)):
            # NOTE: we create a separate `SatelliteViewport per each `Entity assigned
            #   NICE: preserve "pos,vh,margin" as-is, and then reinterpret/resize only when going back
            #   ++ NICE: preserve the *generated* items as-is in the "_wdg._lst"
            #   +++ NICE: can use totally different *widgets* based on the type(_ent)
            #     e.g. Dashboard or Editor
            v = self._cache_pool[nent] = EntityView(nent)
        ## SPLIT?: _append_or_replace(v)
        # NOTE: discard the rest of navi stack if we go into different route (but preserve in _pool)
        # BAD? if we back-n-forth "jump_to(unrelated path)" (e.g. after !ranger),
        #   then that jumped path will be discarded from history :(
        self._view_stack[self._cursor_idx + 1 :] = [v]
        self._cursor_idx = len(self._view_stack) - 1

    # NOTE: can jump to distant/unrelated node in one step
    # BET?RENAME?(jump_to): restore_or_load(ent)
    # RENAME?(:intermediates): {remember,cache,traverse}_parents
    def jump_to(self, nent: Golden, intermediates: bool = False) -> None:
        if isinstance(nent, ErrorEntry):
            log.trace(nent.name)
            return  # FUT: make errors also :Explorable

        # pylint:disable=protected-access
        # if self.focused_view._ent == nent:
        #     log.warning(f"Same: {nent=}")
        #     return
        assert self.focused_view._ent != nent

        if not intermediates:
            self._advance_or_retrieve_or_emplace(nent)

        # CASE:(intermediates=True)::
        if not isinstance(nent, FSEntry):
            raise NotImplementedError(type(nent))
        assert fs.isabs(nent.loci)
        path = fs.normpath(nent.loci)
        # BAD:PERF: we are forced to traverse each intermediate entity
        #   to use the *same* instances of FSEntry as returned by .explore()
        #   ALT~: .rfind() backwards until fs.commonpath(), then append as usual
        self._cursor_idx = 0
        k = 0
        while (k := path.find(fs.sep, k + 1)) > 0:
            pview = self.focused_view
            loci = path[: k or 1]
            found = False
            for i, w in enumerate(pview._wdg._lst):
                if isinstance(w._ent, FSEntry) and w._ent.loci == loci:
                    found = True
                    # SUM: advance inof discarding cached fs.commonpath() parents from left
                    self._advance_or_retrieve_or_emplace(w._ent)
                    # NOTE: set cursor onto entity you came back from
                    wdg = self.focused_view._wdg
                    wdg._viewport_followeditem_lstindex = i
                    wdg._cursor_item_lstindex = i
                    # BAD: hardcoding pos to avoid last item at top
                    pos = pview._wdg._viewport_height_lines // 2
                    wdg._viewport_followeditem_linesfromtop = pos
                    break
            ## IDEA: if "w" not found -- repeat search once again for _orig_lst,
            #     but set cursor on the first visisible item (or completely hide cursor)
            #   NICE: works even if entry was hidden/filtered from .xfm_lst
            # ent = next(x for x in pview._orig_lst if x.loci == loci)
            if not found:
                raise RuntimeError("WTF: node had disappeared")
                self._advance_or_retrieve_or_emplace(FSEntry(loci))

    ## FAIL: `Entry.parent() is not generalizable ※⡧⢃⠬⢖
    ## BET: traverse and preload all intermediate parents on __init__(path)
    ##   NICE: for untraversables (e.g. "mpd://song") we always have preloaded RootEntry as "prev"
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
