from itertools import pairwise
from time import monotonic_ns

from .kernel import MiurKernel
from .systems.tuisystem import VisibleArea
from .uidrv.printtext_drv import PrintTextUIDriver

g_dkpi: dict[str, int] = {}


def kpi(seqnm: str) -> None:
    g_dkpi[seqnm] = monotonic_ns()


def main_navi() -> str:
    k = MiurKernel()
    # h = "/data/g/miur_gen/demo/errors/chained.py"
    nvid = k.new_navi(0, "/etc")
    va = VisibleArea(4, 11)  # OR? use "vpid"
    with PrintTextUIDriver() as ui:
        kpi("bake")
        # THINK: ui.bake() to apply UI-specific constrains ?
        #   BUT: ui-model lives in .kernel, so .drv should be dumb
        #     ~~ unless "baking" inserts colorcodes
        va.wnd_h, va.wnd_w = ui.sizewh()
        va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, va.wnd_h)
        displ, lines = k.navi_sequence(nvid, va)

        kpi("draw")
        ui.draw(lines)
        kpi("done")

    kpistr = " ".join(
        f"{nm}={(t1 - t0) / 1e6:.3f}ms"
        for (nm, t0), (_, t1) in pairwise(sorted(g_dkpi.items(), key=lambda x: x[1]))
    )
    return f"{kpistr} (tokens={len(displ)})"


def main() -> str | None:
    try:
        kpistr = main_navi()
        if "jurigged" in __import__("sys").modules:
            return kpistr
        print(kpistr)
        return None
    except Exception:
        from rich.traceback import install  # PERF:BAD: +400ms

        install(show_locals=True)
        raise
