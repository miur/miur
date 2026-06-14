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
    _uiset = "pr printtext cu curses".split()
    o("--ui", choices=_uiset, default="curses")
    return parser


def main_navi() -> str:
    ns = cli_spec(ArgumentParser()).parse_args(sys.argv[1:])

    ## TODO: multi-window UIDriver spawner for synchronous navigation on side-monitor
    #    USE: spawned term with fd redirect from #miur
    #      /d/miur/legacy/bc5_miur_asyncio/devhelp/newterm.py
    #      /d/miur/src/miur/#/newterm.nou
    match ns.ui:
        case "printtext" | "pr":
            from .uidrv.printtext_drv import PrintTextUIDriver

            UIDrv = PrintTextUIDriver
        case "curses" | "cu":
            from .uidrv.curses_drv import CursesUIDriver

            UIDrv = CursesUIDriver
        case _:
            assert_never(ns.ui)

    process = psutil.Process(os.getpid())

    k = MiurKernel()
    # h = "/data/g/miur_gen/demo/errors/chained.py"
    nvid = k.new_navi(0, "/etc")
    va = VisibleArea(8, 11)  # OR? use "vpid"
    displ: DisplayList = []
    with UIDrv() as ui:
        while True:
            kpi("bake")  # NOTE: overwrites prev values -> so keeps only last frame
            # THINK: ui.bake() to apply UI-specific constrains ?
            #   BUT: ui-model lives in .kernel, so .drv should be dumb
            #     ~~ unless "baking" inserts colorcodes
            va.wnd_w, va.wnd_h = ui.sizewh()
            va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, max(1, va.wnd_h - 1))
            displ = k.navi_sequence(nvid, va)

            # FAIL:(chicken-and-egg problem): drawing time is still unknown
            #   BAD~ show *previous frame* kpi(draw) instead of current one
            kpistr = (
                " ".join(
                    f"{nm}={(t1 - t0) / 1e6:.3f}ms"
                    for (nm, t0), (_, t1) in pairwise(
                        ## DISABLED: order becomes wrong due to draw from prev frame
                        # sorted(g_dkpi.items(), key=lambda x: x[1])
                        ## FIXME: without sorting "bake" is negative
                        g_dkpi.items()
                    )
                )
                + f" (tokens={len(displ)}) Nfd={process.num_fds()}"
            )
            if va.wnd_h > 0:
                displ.append(
                    TextSpan(
                        0,
                        va.wnd_h - 1,
                        kpistr,
                        min(va.wnd_w, width(kpistr)),
                        Aid.footer,
                    )
                )

            kpi("draw")
            # ui.draw_lines(lines)
            ui.draw_displ(displ)

            kpi("input")
            match wch := ui.input():
                case "q":
                    break
                case _:
                    print(wch)
                    pass
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
