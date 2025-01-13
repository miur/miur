import os
import os.path as fs
from typing import Callable, Self, Sequence, override

from .entity_base import Golden
from .entries import FSEntry
from .vlst import SatelliteViewport

# T = TypeVar("T")
# class ListCachingProxy(list[T]):
#     pass


# ALT:SPLIT: make an `EntityContext for serialization/restoration on restart
class EntityView:
    _ent: Golden
    _originator: Self | None
    _wdg: SatelliteViewport
    _act: Callable[[], Sequence[Golden]]
    # _lstpxy: ListCachingProxy[Golden]
    _orig_lst: Sequence[Golden]
    _xfm_lst: list[Golden]
    # TBD: _overlay/_aug_lst -- to inject on-the-fly or from db
    #   NICE:IDEA: show those entries as expanded subtree on node parent level
    #     inof as regular entries inside node itself

    # ALT:BAD?PERF: store in each TextEntity a backref to "originator"
    #   == to be able to return to its "parent"
    #   NICE: only "navigated-to" items will store this backref
    def __init__(self, ent: Golden, originator: Self | None = None) -> None:
        self._ent = ent
        # NOTE: remember which `View have created this one -- tba to return back
        self._originator = originator
        self.fetch()

    @override
    def __repr__(self) -> str:
        ent = self._ent
        olst = self._orig_lst
        xlst = self._xfm_lst
        return "{{" + f"{ent}: {len(xlst)}/{len(olst)}" + "}}"

    def fetch(self) -> None:
        # ALT:PERF(slow): @runtime_checkable : isinstance(ent, Explorable)
        #   https://mypy.readthedocs.io/en/latest/protocols.html#using-isinstance-with-protocols
        if sfn := getattr(self._ent, "explore", None):  # and callable(sfn):
            self._act = sfn  # NOTE: keep sfn to be able to refresh() the list (when externally changed)
            self._orig_lst = self._act()
            self._transform()
            assert getattr(self, "_xfm_lst", None) is not None
            if not getattr(self, "_wdg", None):
                # MAYBE:BET? reuse existing _wdg (from outside `*Layout) inof creating it per each entity
                #   OR:(vice versa): pass whole _view to any existing _wdg to display _lst
                self._wdg = SatelliteViewport()
            self._wdg.assign(self._xfm_lst)
        else:
            raise NotImplementedError()

    # VIZ: sort, reverse, filter, groupby, aug/highlight/mark/tag
    def _transform(self) -> None:
        self._xfm_lst = list(self._orig_lst)
        self._apply_default_policy()

    def _apply_default_policy(self) -> None:
        # pylint:disable=protected-access
        if isinstance(self._ent, FSEntry) and fs.isdir(p := self._ent.loci):
            if os.access(p, os.R_OK):
                os.chdir(p)
            if not fs.islink(p) or self._ent._alt is True:
                # TODO: sort folders before files
                # TODO: sort ignorecase
                self._xfm_lst.sort()
