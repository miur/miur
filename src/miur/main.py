import ctypes
import os
import sys
from argparse import ArgumentParser
from itertools import pairwise
from time import monotonic_ns
from typing import assert_never

import psutil

from .kernel import MiurKernel
from .systems.tuisystem import Aid, DisplayList, TextSpan, VisibleArea, width

g_dkpi: dict[str, int] = {}

if not sys.warnoptions:
    # WHY: print a traceback pinpointing exactly where the unclosed file object was created.
    __import__("warnings").simplefilter("always", ResourceWarning)


# MOVE: logger?
def kpi(seqnm: str) -> None:
    g_dkpi[seqnm] = monotonic_ns()


def cli_spec(parser: ArgumentParser) -> ArgumentParser:
    o = parser.add_argument
    _uiset = "pr printtext cu curses mu multi".split()
    o("--ui", choices=_uiset, default="multi")
    return parser


def main_navi() -> str:
    ns = cli_spec(ArgumentParser()).parse_args(sys.argv[1:])

    match ns.ui:
        case "mu" | "multi":
            from .uidrv.multi_drv import MultiUIDriver

            UIDrv = MultiUIDriver
        case "cu" | "curses":
            from .uidrv.curses_drv import CursesUIDriver

            UIDrv = CursesUIDriver
        case "pr" | "printtext":
            from .uidrv.printtext_drv import PrintTextUIDriver

            UIDrv = PrintTextUIDriver
        case _:
            assert_never(ns.ui)

    process = psutil.Process(os.getpid())

    k = MiurKernel()
    # h = "/data/g/miur_gen/demo/errors/chained.py"
    nvid = k.new_navi(0, "/etc")
    va = VisibleArea(8, 11)  # OR? use "vpid"
    displ: DisplayList = []
    kpistr = ""
    with UIDrv() as ui:
        while True:
            kpi("bake")  # NOTE: overwrites prev values -> so keeps only last frame
            # THINK: ui.bake() to apply UI-specific constrains ?
            #   BUT: ui-model lives in .kernel, so .drv should be dumb
            #     ~~ unless "baking" inserts colorcodes
            va.wnd_w, va.wnd_h = ui.sizewh()
            va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, max(1, va.wnd_h - 1))
            displ = k.navi_sequence(nvid, va)

            kpi("draw")
            ui.clear()
            ui.draw_displ(displ)

            kpi("input")
            match wch := ui.input():
                case "q":
                    break
                case _:
                    pass

            kpi("status")
            ## HACK: draw "status" *after* drawing evels -- to have all actual KPIs
            kpistr = " ".join(
                f"{nm}={(t1 - t0) / 1e6:.3f}ms"
                for (nm, t0), (_, t1) in pairwise(
                    ## DISABLED: order becomes wrong due to draw from prev frame
                    # sorted(g_dkpi.items(), key=lambda x: x[1])
                    ## FIXME: without sorting "bake" is negative
                    g_dkpi.items()
                )
            )
            kpistr = f"{wch!r} {kpistr} (tokens={len(displ)}) Nfd={process.num_fds()}"
            kpiw = min(va.wnd_w, width(kpistr))
            if va.wnd_h > 0:
                ui.draw_displ([TextSpan(0, va.wnd_h - 1, kpistr, kpiw, Aid.footer)])
            ui.refresh()
            kpi("done")

    return k.log.dump() + kpistr


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
