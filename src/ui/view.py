import inspect
import os
from typing import Any, Callable, Iterable, Sequence, override

from ..entity.base import Action, Entities, Entity, Golden, StopExploration
from ..entity.error import ErrorEntry
from ..entity.fsentry import FSDir
from ..entity.text import TextEntry
from ..util.termansi import num_lo, num_up
from .vlst import SatelliteViewport

# T = TypeVar("T")
# class ListCachingProxy(list[T]):
#     pass


## ALT
# try:
#     if __import__("sys").flags.isolated:
#         __import__("site").main()  # lazy init for "site" in isolated mode
#     from just.ext.print import cvt_one
# except ImportError:  # ModuleNotFoundError
#     cvt_one = repr
def cvt_to_ents(x: Any, pview: "EntityView", lvl: str = "") -> Entities:
    if x is None:
        # MAYBE: test if {fun.type_annotations == "-> None"} and don't print anything
        yield ErrorEntry(pview=pview, name="None ∅")
    elif isinstance(x, dict):  # FIXME: or Mapping
        lvl += " "
        for i, k in enumerate(sorted(x), start=1):
            for v in cvt_to_ents(x[k], pview, lvl):
                yield TextEntry(f"{lvl}{num_up(i):>2s} {k}: {v}")
    elif isinstance(x, tuple):
        for a in x:
            for v in cvt_to_ents(a, pview, lvl):
                yield TextEntry(f" ⸱\t{v}")
    elif isinstance(x, (str, int)):
        yield TextEntry(str(x))
    elif isinstance(x, float):
        yield TextEntry(f"{x:.6f}")
    # if isinstance(x, Path):
    #     return repr(str(x))
    # if isinstance(x, (DT, TM, DD)):
    #     return str(x)
    # if isinstance(x, TT):
    #     return dt_hmx(x)
    else:
        try:
            it = iter(x)
        except TypeError:
            for ms, mf in inspect.getmembers(x):
                yield TextEntry(f"{lvl}{ms}: {mf}")
        else:
            lvl += " "
            for i, a in enumerate(it, start=1):
                for v in cvt_to_ents(a, pview, lvl):
                    yield TextEntry(f"{lvl}{num_lo(i):>2s} {v}")


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
            # ALT:PERF(slow): @runtime_checkable : isinstance(ent, Explorable)
            #   https://mypy.readthedocs.io/en/latest/protocols.html#using-isinstance-with-protocols
            methods = inspect.getmembers(self._ent, inspect.ismethod)

            def wrapent(v: Callable[[], Any]) -> Callable[[], Entities]:
                r = inspect.signature(v).return_annotation
                # assert 0, type(r)
                if r in (Entities, Iterable[Entity], Iterable[Golden[Any]]):
                    return v
                if r in (Entity, Golden[Any]):
                    return lambda: [v()]
                # ALT? return ErrorEntry, meaning "you need to generalize your API"
                return lambda: cvt_to_ents(v(), pview=self)

            lst = [
                # IDEA: rename {.explore==.default} to show only "L" as 1st `Action in list
                Action(
                    name=f".{k}()", pview=self, sfn=wrapent(v)
                )  # OR=f"{k.capitalize()}:"
                for k, v in methods
                ## WARN: we also may need to exclude methods which aren't applicable to particular file
                #   ~ no reason to text_lines -- if it's a binary file
                #   ~ no reason to code_syntax_lines -- if syntax isn't supported
                #   ~ no reason to .explore(empty_file) -- unless you trying to insert some new lines
                ## TODO: filter by return type "-> Iterable[Golden]" (or at least "-> Golden" for `Dashboard)
                ##   OR: `Interpret any non-explore as generic TextEntry(str(...))
                if not k.startswith("_")
            ]
            assert lst, methods
            self._act = lambda: lst
        try:
            self._orig_lst = list(self._act())
        # FIXME:ALSO: replace per-item issues with in-list `ErrorEntry
        #   &why to hi-RED files which had disappeared during listing e.g. short-lived prs in /proc/*
        #   &why to show at least partially loaded file from e.g. network
        except StopExploration:
            self._orig_lst = [ErrorEntry(pview=self, name="Atomic(N/A)")]
        except Exception as exc:
            self._orig_lst = [ErrorEntry(pview=self, exc=exc)]
            # raise  # <DEBUG

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
        if (pv := self._ent.pv) and isinstance(pv._ent, FSDir):
            p = pv._ent._x.handle
            if os.access(p, os.R_OK):
                os.chdir(p)
            # if not fs.islink(p) or self._ent._alt is True:
            # TODO: sort folders before files
            # TODO: sort ignorecase
            # TODO: sort by tuple of keys (filetype, name/case, ...)
            self._xfm_lst.sort()
