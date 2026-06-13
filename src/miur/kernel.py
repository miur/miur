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
class MiurKernel:  # pylint:disable=too-many-instance-attributes
    def __init__(self) -> None:
        from .systems.logsystem import log

        ## TBD: query data from one to other connected client-ids
        ##   e.g. viewport-sz, cursors, tab-idx, etc.
        # self.clients = ClientsRegistrySystem(self)
        log.k = self  # HACK: switch "early logs" to "proper ones"
        self.log = log
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
    ) -> tuisystem.DisplayList:
        handle = self._navi[nvid].handle
        displ = self.tui.bake_display_area(handle, va)
        return displ


if TYPE_CHECKING:
    _lfs: LFS.IKernel = MiurKernel()
    _file: FCS.IKernel = MiurKernel()
    _text: textsystem.IKernel = MiurKernel()
    _view: viewsystem.IKernel = MiurKernel()
    _tui: tuisystem.IKernel = MiurKernel()
