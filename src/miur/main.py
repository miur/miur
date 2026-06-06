from time import monotonic_ns

from .kernel import MiurKernel
from .systems.tuisystem import VisibleArea
from .uidrv.printtext_drv import PrintTextUIDriver


def main() -> str | None:
    try:
        perf: list[str] = []
        k = MiurKernel()
        # h = "/data/g/miur_gen/demo/errors/chained.py"
        nvid = k.new_navi(0, "/etc")
        va = VisibleArea(4, 11)  # OR? use "vpid"
        with PrintTextUIDriver() as ui:
            t0 = monotonic_ns()
            ui.bake(k, nvid, va)
            t1 = monotonic_ns()
            ui.draw()
            t2 = monotonic_ns()
            kpi = f"bake={(t1 - t0) / 1e6:.3f}ms draw={(t2 - t1) / 1e6:.3f}ms (tokens={len(ui.displ)})"
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
