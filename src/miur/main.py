import os
from array import array
from typing import assert_never, overload

type Eid = int
type Sid = int
type Oid = int


class OpRef:
    tgt: Eid
    op: Oid


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


# THINK: {combine vs generalize} into `View by adding `MetaHints
#   : DataSchema=rows,kv,text,blob,cells
#   : WidgetHint=list,table,text,hex,image,dashboard
# MAYBE: return "Frames" inof "Pages"
# DECI: make separate `StaticView vs using Listing.reload=None
# RENAME? `{Active,Corporeal}Listing | `PartiallyMaterializableCachedListingAccessor | `PagedRowsView
class SparseListing:
    """NOTE: this is also access model to interact with data -- like SatelliteVP"""

    def __init__(self, k: MiurKernel, op: OpRef) -> None:
        self._k = k  # BAD: circular link
        self._op = op

    @overload
    def __getitem__(self, kx: int, /) -> Eid: ...
    @overload
    def __getitem__(self, kx: slice, /) -> list[Eid]: ...

    # @overload
    # def __getitem__(self, kx: tuple[int] | list[int]) -> list[Eid]: ...

    def __getitem__(self, kx: slice | int, /) -> Eid | list[Eid]:
        if isinstance(kx, int):
            return self._cache[kx]
        if isinstance(kx, slice):
            return self._cache[kx]
        # if isinstance(kx, tuple):  # NICE: get list of items ex~: a[(1,5,8)]
        #     return [self._cache[int(i)] for i in kx]
        # REF:SRC: https://github.com/python/cpython/blob/0c36c37841a66ebfcf1686777b76e7407efbe3ca/Lib/typing.py#L2416
        assert_never(kx)


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
                # OpSpec(self.OP_LISTDIR, 'children', SchemaKind.ROWS, WidgetKind.LIST, OpEffect.OBSERVE, True, True, reopen=ReopenHint.LATEST_FIRST),
                # OpSpec(self.OP_STAT, 'stat', SchemaKind.KV, WidgetKind.TABLE, OpEffect.OBSERVE, True, True, reopen=ReopenHint.LATEST_FIRST),
                self.listdir,
            ]
        raise NotImplementedError


# NEED: `{Root,Entry}System for aliases to all systems and multiple bookmarks
class MiurKernel:  # RENAME? `MiurSystem | `AppContext
    def __init__(self) -> None:
        self._mc = MiurCache()
        self.__lfs = LocalFileSystem()
        self._systems = {self.__lfs.SID: self.__lfs}
        self._protocols = {"file://": self.__lfs.SID}

    def uri_to_eid(self, uri: str) -> Eid:
        for pfx in self._protocols:
            if uri.startswith(pfx):
                sid = self._protocols[pfx]
                handle = uri.removeprefix(pfx)
                # TBD: for stable-ish handles -- return same .eid if it's already cached
                #   FAIL? .path may refer to smth else if smb will over-mount it between calls
                return self._mc.add(sid=sid, dat=handle)
        raise NotImplementedError

    def ops_for(self, eid: Eid):  # -> [OpSpec]
        system = self._systems[self._mc.sid[eid]]
        handle = self._mc.dat[eid]
        return system.ops_for(str(handle))  # BAD: forced to typecast

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
    def apply_op(self, eid: Eid, op: Oid):
        # kid = k.store.kid_of(op.target)
        # pid, _p1, _p2 = k.store.payload(op.target)
        # path = k.strings.get(int(pid))
        # if kid == self.K_DIR and op.op == self.OP_LISTDIR:
        #     return open_listdir(self, k, path, title=path)
        # pg = lfs.listdir(d)
        return SparseListing(self._lfs.listdir(path))


# TBD: there can be multiple cursors (e.g. on <C-n>), so cursors themselves should be stored
#   in a separate system, promoting their items to persistent storage and UI should consult
#   this system to know which items how to highlight
class SatelliteVP:  # RENAME? SatelliteViewportModel
    """Stateful cursor access model"""

    lst: SparseListing
    start: int

    def bind(self, lst: SparseListing, start: int = 0) -> None:
        self.lst = lst
        self.start = start

    # FUT: impl __next__() only if you wish to expose some functions mid-iteration for e.g. `LazyListings
    # CHG?MAYBE: it should return generic "objects" and then UI decides by itself how to represent them
    def __iter__(self):
        yield from self.lst[self.start :]


# RENAME? {Stringed,Linked}PanelNavi
class PentaPanelNavi:
    """Encapsulates user's mental model of navigation continuity, memorized context and history"""

    # IDEA: default size == 1 cell -- so we could jump/scroll to any item even before first .resize()
    def __init__(self, k: MiurKernel, eid: Eid) -> None:
        # BAD?DECI: should we create SatelliteVP per visited node and reassign widgets on move,
        #   OR:BET? precreate fixed 5 SatelliteVP store only compact navi struct and update params?
        #     << seems much more memory-efficient -- and we could keep those data cached even if listing itself was evicted
        #     ALSO:WARN: inotify() hooks (limited number) should belong to `*Navi
        #       inof activating/deactivating them per each cached entity view
        #   MAYBE: we don't even need all panels be full-fledged SatelliteVP !
        #     >> then they also could be independent and/or drawn by RootTUI.draw() directly instead of being governed here
        self.browse = SatelliteVP()  # RENAME? .view / .c[entral]view
        # CHG: avoid hardcoding -- wrap .ops_for() into generic OpRef part of MetaSystem
        self.browse.bind(k.ops_for(eid))

    ## MAYBE:HYPO: there is no "jumping without reason" in navi model -- all actions are traceable
    #   >> which means there is nowhere for argument to jump_to() to come from
    #   * if "path" comes from clipboard -- it means in current folder you access clipboard, treat it as path or
    #     you treat it as text where you pick the path, and then navigate there, keeping these choices for backtracking
    #   * if you input path in the prompt -- it's the same, you basically make choice in current folder,
    #     even if you use smth akin ":cd /path/to" it's like applying function with supplied arg1 to current folder
    # RENAME: .switch_to() -- meaning instant/"un-continuous" compared to "jump[ing-over]_to"
    # def jump_to(self, d: str) -> None:
    #     pass

    def go_into(self) -> None:
        pass

    def go_back(self) -> None:
        pass


class RootTUI:
    def __init__(self, nav: PentaPanelNavi) -> None:
        self.nav = nav  # RENAME? .ctl

    def draw(self) -> None:
        ## TODO: generalize printing api themselves
        # StdoutStreamDriver.add = print
        ## NOTE! ui/layout governs where to place each panel -- NOT .navi itself
        #   >> so, layout should *know* all panels names of .navi
        print(" ".join(str(x) for x in self.nav.browse))


def main() -> None:
    # print("[miur]")
    k = MiurKernel()
    eid = k.uri_to_eid("file:///data/g/miur_gen/demo")
    nav = PentaPanelNavi(k, eid)
    ui = RootTUI(nav)
    ui.draw()
