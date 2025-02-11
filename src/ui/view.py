import inspect
import os
from typing import Any, Callable, Iterable, Sequence, override

from ..entity.base import Action, Entities, Entity, Golden, StopExploration
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
    _orig_lst: Sequence[Golden[Any]]
    _xfm_lst: list[Golden[Any]]
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

        self._transform()
        assert getattr(self, "_xfm_lst", None) is not None
        if not getattr(self, "_wdg", None):
            self._wdg = self._wdgfactory()
        self._wdg.assign(self._xfm_lst)
        self._visited = False

    # VIZ: sort, reverse, filter, groupby, aug/highlight/mark/tag
    def _transform(self) -> None:
        self._xfm_lst = list(self._orig_lst)
        self._apply_default_policy()

    def _apply_default_policy(self) -> None:
        # pylint:disable=protected-access
        if isinstance((pent := self._ent.parent), FSDir):
            p = pent._x.handle
            if os.access(p, os.R_OK):
                os.chdir(p)
            # if not fs.islink(p) or self._ent._alt is True:
            # TODO: sort folders before files
            # TODO: sort ignorecase
            # TODO: sort by tuple of keys (filetype, name/case, ...)
            self._xfm_lst.sort()
