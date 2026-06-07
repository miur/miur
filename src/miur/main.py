import ctypes
import sys
from itertools import pairwise
from time import monotonic_ns

from .kernel import MiurKernel
from .systems.tuisystem import VisibleArea
from .uidrv.curses_drv import CursesUIDriver

g_dkpi: dict[str, int] = {}


def kpi(seqnm: str) -> None:
    g_dkpi[seqnm] = monotonic_ns()


def main_navi() -> str:
    k = MiurKernel()
    # h = "/data/g/miur_gen/demo/errors/chained.py"
    nvid = k.new_navi(0, "/etc")
    va = VisibleArea(4, 11)  # OR? use "vpid"
    ## TODO: multi-window UIDriver for synchronous navigation on side-monitor
    # from .uidrv.printtext_drv import PrintTextUIDriver
    # with PrintTextUIDriver() as ui:
    with CursesUIDriver() as ui:
        kpi("bake")
        # THINK: ui.bake() to apply UI-specific constrains ?
        #   BUT: ui-model lives in .kernel, so .drv should be dumb
        #     ~~ unless "baking" inserts colorcodes
        va.wnd_w, va.wnd_h = ui.sizewh()
        va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, va.wnd_h)
        displ, _lines = k.navi_sequence(nvid, va)

        kpi("draw")
        # ui.draw_lines(lines)
        ui.draw_displ(displ)
        kpi("done")
        input()

    kpistr = " ".join(
        f"{nm}={(t1 - t0) / 1e6:.3f}ms"
        for (nm, t0), (_, t1) in pairwise(sorted(g_dkpi.items(), key=lambda x: x[1]))
    )
    return f"{kpistr} (tokens={len(displ)})"


def main() -> str | None:
    try:
        # FIXED: $ pkill miur  ALT: https://pypi.org/project/setproctitle/
        if sys.platform == "linux":
            appname = "miur"
            maxcommlen = 15
            assert len(appname) <= maxcommlen, "limit for /proc/PID/comm"
            libc = ctypes.CDLL("libc.so.6")
            PR_SET_NAME = 15
            libc.prctl(PR_SET_NAME, appname.encode("utf-8"), 0, 0, 0)

        kpistr = main_navi()
        if "jurigged" in __import__("sys").modules:
            return kpistr
        print(kpistr)
        return None
    except Exception:
        from rich.traceback import install  # PERF:BAD: +400ms

        install(show_locals=True)
        raise
