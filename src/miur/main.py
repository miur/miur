import shutil
from time import monotonic_ns
from typing import TYPE_CHECKING

from .systems import filecontentsystem as FCS
from .systems import localfilesystem as LFS
from .systems import textsystem, tuisystem, viewsystem


# THINK: do we even need all these classes, or C-like global funcs will do?
class MiurKernel:
    def __init__(self) -> None:
        self.lfs = LFS.LocalFileSystem(self)
        self.file = FCS.FileContentSystem(self)
        self.text = textsystem.TextSystem(self)
        self.view = viewsystem.ViewSystem(self)
        self.tui = tuisystem.TuiSystem(self)

    def list_systems(self) -> list[str]:
        return [k for k, v in vars().items() if v.__class__.__name__.endswith("System")]

    # def exec(self, op: object) -> object:
    #     return None

    # TODO? NaviSystem
    # TODO: CentralPipeline/Sequence
    def navi_sequence(
        self,
        handle: str,
        va: tuisystem.VisibleArea,
    ) -> tuple[tuisystem.DisplayList, list[str]]:
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
    def redraw(self, kernel: MiurKernel, handle: str, va: tuisystem.VisibleArea) -> str:
        t0 = monotonic_ns()
        va.wnd_w, va.wnd_h = shutil.get_terminal_size(fallback=(80, 24))
        va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, va.wnd_h)
        displ, strings = kernel.navi_sequence(handle, va)
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
        h = "/etc"
        va = tuisystem.VisibleArea(4, 11)
        perf.append(UI().redraw(k, h, va))

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
