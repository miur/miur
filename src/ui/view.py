import inspect
import os
from typing import Any, Callable, Iterable, Sequence, override

from ..entity.base import Action, Entities, Entity, StopExploration
from ..entity.error import ErrorEntry
from ..entity.fsentry import FSDir
from ..entity.objaction import pyobj_to_actions
from ..util.logger import log
from .vlst import SatelliteViewport

# T = TypeVar("T")
# class ListCachingProxy(list[T]):
#     pass


# ALT:SPLIT: make an `EntityContext for serialization/restoration on restart
class EntityView:
    _wdg: SatelliteViewport
    # NOTE:(_act): keep =sfn to be able to refresh() the list (when externally changed)
    _act: Callable[[], Entities]
    # _lstpxy: ListCachingProxy[Golden]
    _orig_lst: Sequence[Entity]
    _xfm_lst: list[Entity]
    _visited: bool | None
    # TBD: _overlay/_aug_lst -- to inject on-the-fly or from db
    #   NICE:IDEA: show those entries as expanded subtree on node parent level
    #     inof as regular entries inside node itself

    # ALT:BAD?PERF: store in each TextEntity a backref to "originator"
    #   == to be able to return to its "parent"
    #   NICE: only "navigated-to" items will store this backref
    def __init__(
        self,
        ent: Entity,
        wdgfactory: Callable[[], SatelliteViewport],
        # originator: Self | None = None,  ## ALT: parent's `EntityView == self._ent.pv
    ) -> None:
        self._ent = ent
        self._wdgfactory = wdgfactory
        # NOTE: remember which `View have created this one -- tba to return back
        # self._originator = originator
        self._filterby = ""
        self._sortby = "unchanged"
        self._sortrev = False
        self.fetch()
        # ALG: re-assign to None on ctor, and then to False each update by .fetch()
        self._visited = None

    @override
    def __repr__(self) -> str:
        ent = self._ent
        olst = self._orig_lst
        xlst = self._xfm_lst
        # return "{{" + f"{ent}={len(xlst)}/{len(olst)}" + "}}"
        return f"V({ent},{len(xlst)}/{len(olst)})"

    def fetch(self) -> None:
        if isinstance(self._ent, Action):
            self._act = self._ent.explore  # CHG=_default()
        else:
            self._act = lambda: pyobj_to_actions(self._ent, parent=self._ent)
        try:
            self._orig_lst = list(self._act())
        # FIXME:ALSO: replace per-item issues with in-list `ErrorEntry
        #   &why to hi-RED files which had disappeared during listing e.g. short-lived prs in /proc/*
        #   &why to show at least partially loaded file from e.g. network
        except StopExploration:
            # FIXME: yield true NonExplorableAtomic() to prevent recursive expansion here
            self._orig_lst = [ErrorEntry(parent=self._ent, name="Atomic(N/A)")]
        except Exception as exc:
            self._orig_lst = [ErrorEntry(parent=self._ent, exc=exc)]
            # from traceback import format_exception
            # for l in format_exception(exc, chain=True):
            #     log.error(l)
        self._apply_default_policy()

        if not getattr(self, "_wdg", None):
            self._wdg = self._wdgfactory()
        self._transform()
        self._visited = False

    def _apply_default_policy(self) -> None:
        # pylint:disable=protected-access
        if isinstance((pent := self._ent.parent), FSDir):
            p = pent._x.handle
            if os.access(p, os.R_OK):
                os.chdir(p)
            # if not fs.islink(p) or self._ent._alt is True:
            self._sortby = "name"
            self._sortrev = False

    # TODO: rgx,glob,patt,stem,substr,words,fuzzy,...
    def filter_by(self, needle: str) -> None:
        if needle == self._filterby:
            return
        self._filterby = needle
        # BAD:PERF: i.e. when we add letter to patt(non-rgx) -- number of results
        #   will be *less* and never *more* (BUT: for look-ahead rgx it may be different)
        self._transform()

    ## RND:ARCH:BET: pass tuple of strategies inof some combined str syntax
    #   TODO:(strategy): allow sorting by tuple of keys/fns (filetype, name/case, ...)
    #   TODO: add strategy to sort folders before files
    #   ADD: ignorecase/smartcase/unicode_normalize/skipdelim("_-")/skipspace
    def order_by(self, strategy: str) -> None:
        if self._sortby == strategy:
            return
        self._sortby = strategy
        self._transform()

    # RENAME? asc, desc, toggle/flip
    def order_rev(self, rev: bool | None = None, /) -> None:
        if rev is None:
            self._sortrev = not self._sortrev
        elif self._sortrev != rev:
            self._sortrev = rev
        else:
            return
        self._transform()

    # [_] SEE: how I did in previous incarnations of !miur/!pa3arch/etc
    # VIZ: sort, reverse, filter, groupby, aug/highlight/mark/tag
    def _transform(self) -> None:
        # NOTE: avoid printing status from ctor(), but print on each change
        if hasattr(self, "_xfm_lst"):
            log.info(f"orderby{self._sortby}{"-" if self._sortrev else "+"}")
        lst = list(self._orig_lst)
        ## DISABLED: we should allways preserve orig list order (to be able to restore it)
        ##   NICE: when toggling reverse, we don't need to track the _orig_lst current rev state
        # if not isinstance(lst, list):
        #     lst = list(lst)

        if substr := self._filterby:
            lst = [e for e in lst if substr in e.name]
        else:
            # RND: to eliminate printing unnecessary local var during exception
            del substr

        # BET? use SortStrategyEnum to detect errors (inof str)?
        #   BAD: we will need to import that `Enum
        ss = self._sortby
        # ALT:RENAME? {"name" -> "default"} -- meaning DFL for generic `Entities
        if ss == "name":
            lst.sort(reverse=self._sortrev)
        elif ss == "unchanged":
            if self._sortrev:
                lst.reverse()
            log.info(lst[0])
        # elif ss == "size":
        #     lst.sort(key=lambda e: len(self._pool[e]._vlst). reverse=rev)
        else:
            raise NotImplementedError

        self._xfm_lst = lst
        self._wdg.assign(self._xfm_lst)
