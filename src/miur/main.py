import shutil
from time import monotonic_ns
from typing import TYPE_CHECKING, Protocol

from .systems import filecontentsystem as FCS
from .systems import localfilesystem as LFS
from .systems import sessionstatesystem as SSS
from .systems import textsystem, tuisystem, viewsystem

if TYPE_CHECKING:

    class IKernel(Protocol):
        pass


type NaviId = int


class NaviSessionSystem:
    def __init__(self, kernel: IKernel, handle: str) -> None:
        self.k = kernel
        self._h = handle  # RENAME? .root_handle

    @property
    def handle(
        self,
    ) -> str:  # RENAME? .current_handle OR return NamedTuple:current.handle
        return self._h


# THINK: do we even need all these classes, or C-like global funcs will do?
class MiurKernel:
    def __init__(self) -> None:
        ## TBD: query data from one to other connected client-ids
        ##   e.g. viewport-sz, cursors, tab-idx, etc.
        # self.clients = ClientsRegistrySystem(self)
        self._sess: list[SSS.SessionStateSystem] = []
        self._navi: list[NaviSessionSystem] = []
        self.lfs = LFS.LocalFileSystem(self)
        self.file = FCS.FileContentSystem(self)
        self.text = textsystem.TextSystem(self)
        self.view = viewsystem.ViewSystem(self)
        self.tui = tuisystem.TuiSystem(self)

    def list_systems(self) -> list[str]:
        return [k for k, v in vars().items() if v.__class__.__name__.endswith("System")]

    # def exec(self, op: object) -> object:
    #     return None

    # def connect(self, client/clientid) -> ClientId:
    #     pass
    # def negotiate_protocols(self, clientid):
    #     pass
    # def new_session(self) -> SSS.SessionId:
    #     sess = SSS.SessionStateSystem(self)
    #     ssid = len(self._sess)
    #     self._sess.append(sess)
    #     return ssid

    def new_navi(self, _ssid: SSS.SessionId, handle: str) -> NaviId:
        navi = NaviSessionSystem(self, handle)
        nvid = len(self._navi)
        self._navi.append(navi)
        # self._sess[ssid].assoc_navi(nvid)
        return nvid

    # TODO? CentralPipeline/Sequence
    #   ALSO: beside SEQ=session-navi we may have stateless-ops
    def navi_sequence(
        self,
        nvid: NaviId,
        va: tuisystem.VisibleArea,
    ) -> tuple[tuisystem.DisplayList, list[str]]:
        handle = self._navi[nvid].handle
        displ = self.tui.bake_display_area(handle, va)
        strings = self.tui.render_term_strings(displ)
        return displ, strings


if TYPE_CHECKING:
    _lfs: LFS.IKernel = MiurKernel()
    _file: FCS.IKernel = MiurKernel()
    _text: textsystem.IKernel = MiurKernel()
    _view: viewsystem.IKernel = MiurKernel()
    _tui: tuisystem.IKernel = MiurKernel()


class UI:
    def redraw(
        self, kernel: MiurKernel, nvid: NaviId, va: tuisystem.VisibleArea
    ) -> str:
        t0 = monotonic_ns()
        va.wnd_w, va.wnd_h = shutil.get_terminal_size(fallback=(80, 24))
        va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, va.wnd_h)
        displ, strings = kernel.navi_sequence(nvid, va)
        t1 = monotonic_ns()
        # NOTE: separate print() delays from frame preps measurement
        mydrv_print = print
        mydrv_print("".join(strings))
        t2 = monotonic_ns()
        return f"dt={(t1 - t0) / 1e6:.3f}ms [+{(t2 - t1) / 1e6:.3f}ms] (tokens={len(displ)})"


def main() -> str | None:
    try:
        perf: list[str] = []
        k = MiurKernel()
        # h = "/data/g/miur_gen/demo/errors/chained.py"
        nvid = k.new_navi(0, "/etc")
        va = tuisystem.VisibleArea(4, 11)  # OR? use "vpid"
        kpi = UI().redraw(k, nvid, va)
        perf.append(kpi)

        # handle = "/etc"
        # names = list(sorted(k.lfs_listdir(handle)))
        # perf.append(UI().redraw(names))
        #
        # focused = next(nm for nm in names if nm.startswith("av"))  # TEMP
        #
        # subhdl = handle + "/" + focused
        # subdir = list(sorted(k.lfs_listdir(subhdl)))
        # UI().redraw(subdir)
        # perf.append(UI().redraw(subdir))
        perfstr = " | ".join(perf)
        if "jurigged" in __import__("sys").modules:
            return perfstr
        print(perfstr)
        return None

    except Exception:
        from rich.traceback import install  # PERF:BAD: +400ms

        install(show_locals=True)
        raise
