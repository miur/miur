import os
from array import array
from dataclasses import dataclass
from time import monotonic_ns
from typing import Iterator, cast  # assert_never, overload

type Eid = int
type Sid = int
type Oid = str  # TEMP?CHG> int


# TODO: paged storage should be opaque
#   * pgsz may be changed later, based on mgmt metadata overhead
#   * compacting multiple small pgs should be seamless
#   * deleting individual entities and compacting may be a thing
#     == at least for persistent/promoted db
class MiurCache:
    cap: int = 100  # RENAME? .prealloc

    def __init__(self) -> None:
        self.last = 0
        # tc = 'I' if array('I').itemsize == 4 else 'L'
        self.sid = array("H", [0]) * self.cap
        self.dat: list[object] = [None] * self.cap

    def __len__(self) -> int:
        return self.last

    # CHG: make handle into u64 ?
    def add(self, *, sid: Sid, dat: object) -> Eid:
        slot = self.last + 1
        if slot >= self.cap:
            raise OverflowError(slot)
        self.sid[slot] = sid
        self.dat[slot] = dat
        self.last = slot
        return slot


# ARCH: use "int" to make OpRef hashable -- to be used as key for `Listing's cache
#   MAYBE: combine sid+op into single "int" for "global namespace of ops"
@dataclass(frozen=True, slots=True, kw_only=True)
class OpRef:
    h: object  # .handle
    sid: Sid  # .system_id
    oid: Oid  # .operation_id
    # _: KW_ONLY
    # args: bytes = b""
    # kw/params: bytes = b""


# THINK: {combine vs generalize} into `View by adding `MetaHints
#   : DataSchema=rows,kv,text,blob,cells
#      BAD? same data can be re-interpreted as different schema, so "hint" is somewhat useless ?
#   : WidgetHint=list,table,text,hex,image,dashboard
#      FAIL: ui/policy should decide widget based on *datatype/schema*, hint doesn't matter ?
# MAYBE: return "Frames" inof "Pages"
# DECI: make separate `StaticView vs using Listing.reload=None
# RENAME? `{Active,Corporeal}Listing | `PartiallyMaterializableCachedListingAccessor | `PagedRowsView
# class SparseListing:
#     """NOTE: this is also access model to interact with data -- like SatelliteVP"""
class OpResult:  # RENAME? Snapshot, View, Result, RetData, Data
    # mime: "blob/ELFFile" | "text/lines"

    def __init__(
        self,
        data: object,  # RENAME? .out[put]
        op: OpRef | None = None,  # THINK: do I even need to store it ?
        ts: int | None = None,
        dt: int | None = None,
    ) -> None:
        # self._k = k  # BAD: circular link
        self.data = data
        self.op = op
        self.ts = ts
        self.dt = dt

    # @overload
    # def __getitem__(self, kx: int, /) -> Eid: ...
    # @overload
    # def __getitem__(self, kx: slice, /) -> list[Eid]: ...
    # @overload
    # def __getitem__(self, kx: tuple[int] | list[int]) -> list[Eid]: ...
    # def __getitem__(self, kx: slice | int, /) -> Eid | list[Eid]:
    #     if isinstance(kx, int):
    #         return self.data[kx]
    #     if isinstance(kx, slice):
    #         return self.data[kx]
    #     # if isinstance(kx, tuple):  # NICE: get list of items ex~: a[(1,5,8)]
    #     #     return [self._cache[int(i)] for i in kx]
    #     # REF:SRC: https://github.com/python/cpython/blob/0c36c37841a66ebfcf1686777b76e7407efbe3ca/Lib/typing.py#L2416
    #     assert_never(kx)

    def __iter__(self) -> Iterator[object]:
        data = self.data
        if isinstance(data, (list, tuple)):
            # yield from data[:]
            return iter(cast(list[object], data))
        # THINK: should I auto-fallback to splitting text into iterable ?
        #   ALT? error-out and demand explicit typecasting or changing MIME or applying `Projection
        if isinstance(data, str):
            return iter(data.splitlines())
        # assert_never(str(type(data)))
        raise TypeError(type(data))


# THINK: should I create different view per sorting/filtering ?
#   = multiple sorted views are confusing
#     -- unless user explicitly demands new views -- to compare them side-by-side
#       << in cloned tab OR by gathering/copying marked listings into same virtual view
#     ~~ e.g. implicitly creating a new view could be done by:
#       :: view.element(listing).order_by(size)
#         BAD: some "elements" come from view widget itself! -- and not only entity and global context
class RowsView:
    order_by: None = None

    def __init__(self, r: OpResult) -> None:
        self._lst = list(r)
        if self.order_by:
            self._lst.sort(key=self.order_by)  # OR key=str

    def __iter__(self) -> Iterator[object]:
        return iter(self._lst)

    def __getitem__(self, kx: slice, /) -> list[object]:
        return self._lst[kx]


# BAD: we may still need FSDir/FSFile classes/wrappers to split API
#   ~~ especially if we go with ELFFile as it has multiple levels of structs with very different available ops
#   OR? we may make system return different ops based on entity type ?
class LocalFileSystem:
    SID = 11

    def listdir(self, path: str) -> tuple[os.DirEntry[str], ...]:
        # TODO: allow chunked loading for very large directories to avoid waiting full list
        with os.scandir(path) as it:
            return tuple(it)

    def ops_for(self, path: str) -> list[object]:  # -> [OpSpec]
        # ALT: if k.store.kid_of(eid) == self.K_DIR:
        if os.path.isdir(path):
            # if os.path.islink(path): ops.extend(link_ops(self))
            return [
                ## REF: /data/g/miur_gen/src/miur/systems/localfs/system.py:107
                # OpSpec(self.OP_LISTDIR, 'children', SchemaKind.ROWS, WidgetKind.LIST,
                #   OpEffect.OBSERVE, True, True, reopen=ReopenHint.LATEST_FIRST),
                # OpSpec(self.OP_STAT, 'stat', SchemaKind.KV, WidgetKind.TABLE,
                #   OpEffect.OBSERVE, True, True, reopen=ReopenHint.LATEST_FIRST),
                self.listdir,
            ]
        raise NotImplementedError


# NEED: `{Root,Entry}System for aliases to all systems and multiple bookmarks
class MiurKernel:  # RENAME? `MiurSystem | `AppContext
    def __init__(self) -> None:
        self._mc = MiurCache()
        # IDEA: lazy factory -- make system on first access to .SID -- inof making all systems by default
        lfs = LocalFileSystem()
        self._systems = {lfs.SID: lfs}
        self._protocols = {"file://": lfs.SID}

    def uri_to_opref(self, uri: str) -> OpRef:
        for pfx in self._protocols:
            if uri.startswith(pfx):
                sid = self._protocols[pfx]
                system = self._systems[sid]
                # DECI: parse params tail here or pass to LocalFS for system-specific proto parsing
                #   ex~: "/path/to/dir?op=listdir&ord=asc&grp=dirs"
                handle, _, params = uri.removeprefix(pfx).partition("?")
                kwargs = (
                    dict(kv.split("=") for kv in params.split("&")) if params else {}
                )
                oid = kwargs.get("op", system.listdir.__name__)
                # TBD: for stable-ish handles -- return same .eid if it's already cached
                #   FAIL? .path may refer to smth else if smb will over-mount it between calls
                # return self._mc.add(sid=sid, dat=handle)
                return OpRef(h=handle, sid=sid, oid=oid)
        raise NotImplementedError

    # def ops_for(self, eid: Eid):  # -> [OpSpec]
    #     system = self._systems[self._mc.sid[eid]]
    #     handle = self._mc.dat[eid]
    #     return system.ops_for(str(handle))  # BAD: forced to typecast

    # OLD:(gen): LocalFsSystem.open(self, k, op: OpRef, *, intent: Intent, sess: int = 0) -> View:
    # CHG? .exec(self, opref) -- COS .sid is already encoded in .eid
    #   TODO: keep ids as opaque as possible (to postpone arch decisions)
    #     -- either separate them or combine into opaque handle
    #   * eid *belongs* to System -- COS its payload(handle) only interpretable by `System
    #   * opid also belongs to System -- COS only system has body to apply the op
    #   * available ops list depends on both eid.sid *and* eid.payload (ELFFile)
    #     so ops-lists are unique/regeneratable
    #   * interp -- we could still apply any wrong op and hope for the best
    #   * listing may consist of ops names -- and apply them to .eid on go_into(),
    #     or it may use oprefs -- to also bind them with args to make them 0-arity,
    #     as for virtual lists -- oprefs may need to bind ops to .eid beforehand
    #       >> TODO: when you copy/promote entity -- make it into full opref
    # def apply_op(self, eid: Eid, op: Oid):
    #     # kid = k.store.kid_of(op.target)
    #     # pid, _p1, _p2 = k.store.payload(op.target)
    #     # path = k.strings.get(int(pid))
    #     # if kid == self.K_DIR and op.op == self.OP_LISTDIR:
    #     #     return open_listdir(self, k, path, title=path)
    #     # pg = lfs.listdir(d)
    #     return SparseListing(self._lfs.listdir(path))
    def exec(self, op: OpRef) -> OpResult:
        system = self._systems[op.sid]
        fn = getattr(system, op.oid)
        t0 = monotonic_ns()
        data = fn(op.h)
        t1 = monotonic_ns()
        return OpResult(data=data, op=op, ts=t1, dt=t1 - t0)


# TBD: there can be multiple cursors (e.g. on <C-n>), so cursors themselves should be stored
#   in a separate system, promoting their items to persistent storage and UI should consult
#   this system to know which items how to highlight
class SatelliteVP:  # RENAME? SatelliteViewportModel
    """Stateful cursor access model"""

    view: RowsView
    start: int

    # RENAME? .watch / .attach / .connect / .bind
    # WARN: satellite uses .view inof .result COS it scrolls over sort/filter/flatten items !
    #   ALSO: views could be converted into each other by `Projection or reinterpretation
    #     e.g. :kv or :table or even :text or :hex -> rasterized into regular :rows
    def assign(self, view: RowsView, start: int = 0) -> None:
        self.view = view
        self.start = start

    # FUT: impl __next__() only if you wish to expose some functions mid-iteration for e.g. `LazyListings
    # CHG?MAYBE: it should return generic "objects" and then UI decides by itself how to represent them
    def __iter__(self) -> Iterator[object]:
        yield from self.view[self.start :]


# RENAME? {Stringed,Linked}PanelNavi | PentaChain
class PentaPanelNavi:
    """Encapsulates user's mental model of navigation continuity, memorized context and history"""

    # IDEA: default size == 1 cell -- so we could jump/scroll to any item even before first .resize()
    def __init__(self) -> None:
        # BAD?DECI: should we create SatelliteVP per visited node and reassign widgets on move,
        #   OR:BET? precreate fixed 5 SatelliteVP store only compact navi struct and update params?
        #     << seems much more memory-efficient
        #       ++ and we could keep those data cached even if listing itself was evicted
        #     ALSO:WARN: inotify() hooks (limited number) should belong to `*Navi
        #       inof activating/deactivating them per each cached entity view
        #   MAYBE: we don't even need all panels be full-fledged SatelliteVP !
        #     >> then they also could be independent and/or drawn by RootTUI.draw() directly
        #       instead of being governed here
        self.browse = SatelliteVP()  # RENAME? .view / .c[entral]view

    ## MAYBE:HYPO: there is no "jumping without reason" in navi model -- all actions are traceable
    #   >> which means there is nowhere for argument to jump_to() to come from
    #   * if "path" comes from clipboard -- it means in current folder you access clipboard,
    #     treat it as path or you treat it as text where you pick the path,
    #     and then navigate there, keeping these choices for backtracking
    #   * if you input path in the prompt -- it's the same, you basically make choice in current folder,
    #     even if you use smth akin ":cd /path/to" it's like applying function
    #     with supplied arg1 to current folder
    # RENAME: .switch_to() -- meaning instant/"un-continuous" compared to "jump[ing-over]_to"
    # def jump_to(self, d: str) -> None: pass
    # WARN! .assign is NOT .jump_to -- it's completely disjoint
    #   and may even reconstruct parent history from scratch
    #   MAYBE: don't allow .assign at all -- and pass it solely through ctor ?
    def assign(self, view: RowsView) -> None:
        # MAYBE? run OpRef directly here -- as we anyway need to run OpRef for preview()
        #   WHY:ARCH: *navigation* is an *act of auto-applying ops*, so it's logical to belong to `*Navi
        #     ~~ scrolling SatVP does nothing by itself -- but preview should be changed after cursor moved
        #       ? should we wrap cursor_up/down() to allow updating preview afterwards ?
        #       ? OR:BET? should we subscribe `Navi to signal from SatVP when it moves ?
        #   BUT:ALT: we may still wish to show *arbitrary* hand-crafted view here,
        #     making `Navi into interpreter of whatever supplied contents
        self.browse.assign(view)
        # self.preview.assign( RowsView( k.exec( OpRef(
        #   h=self.browse.focused_item.handle, sid=k.policy.same, op="preview"))))

    def go_into(self) -> None:
        # h = self.browse.focused_item  # << primary cursor [of many]
        # view = k.make_view(h, sid, op={ops_for|selectors|opid...})
        #   BUT! view should have datatype_hint, which depends on op!
        # self.browse.assign(view)
        #     >> view.materialize() in .browse itself !, only declare here
        pass

    def go_back(self) -> None:
        pass

    def cursor_step_by(self, _steps: int) -> None:
        # ALT: self.browse.bus.send_event(self.go_up)
        #   WHY:BET: moving cursor up/down will be solely through .browse API
        #   BUT:BAD: moving up/dn parent (and most other keybinds) would still involve .navi :(
        pass


class RootTUI:
    def __init__(self, nav: PentaPanelNavi) -> None:
        self.nav = nav  # RENAME? .ctl

    def draw(self) -> None:
        ## TODO: generalize printing api themselves
        # StdoutStreamDriver.add = print
        ## NOTE! ui/layout governs where to place each panel -- NOT .navi itself
        #   >> so, layout should *know* all panels names of .navi
        items: list[str] = []
        for x in self.nav.browse:
            nm = str(x.name) if isinstance(x, os.DirEntry) else str(x)
            items.append(nm)
        print(" ".join(items))


def main() -> None:
    k = MiurKernel()
    op = k.uri_to_opref("file:///data/g/miur_gen/demo?op=listdir")
    # CHG: avoid hardcoding -- wrap .ops_for() into generic OpRef part of MetaSystem
    snap = k.exec(op)  # RENAME? result, ret, lst, value
    view = RowsView(snap)
    nav = PentaPanelNavi()
    nav.assign(view)
    ui = RootTUI(nav)
    ui.draw()
