import ctypes
import curses
import os
import signal
import sys
from argparse import ArgumentParser
from itertools import pairwise
from time import monotonic_ns
from typing import assert_never

import jurigged
import psutil
from jurigged.codetools import UpdateOperation

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
        ## BAD: jurigged produces multiple logs per single saved file --> screen refreshes N-times
        ##   TODO: spawn/reset 500ms timer after each event, and only refresh when timer is done

        def jurigged_on_event(event: object) -> None:
            k.log.info(event)
            # OR: if "Evaluating" in str(event) or "Update" in str(event): pass; else: return
            if isinstance(event, UpdateOperation):
                ## DISABLED:FAIL: doesn't unblock already waiting getch()
                # try:
                #     # HACK: unblock current .getch()
                #     # curses.ungetch(curses.KEY_REFRESH)
                #     # curses.ungetch(curses.KEY_RESIZE)  # safe
                #     # OR:(^L=12): ungetch(12) | ungetch(curses.KEY_F5)
                # except curses.error:
                #     pass
                ## FIXED:WKRND: Send a native signal to wake up the main thread's getch()
                os.kill(os.getpid(), signal.SIGWINCH)

        ### HACK: hot-reload (recursive ./*.py files from PWD?)
        ## CHECK: if it has import-hook to discover lazily loaded modules later
        # OR: jurigged.watch(pattern=[fs.dirname(fs.realpath(__file__)) + "/**/*.py"], logger=jurigged_on_event)
        jurigged.watch(logger=jurigged_on_event)  # <CASE: recursive
        # jurigged.watch("miur") # <OR watch a specific package directory (non-recursive)
        # import mymod; jurigged.watch(mymod) # <OR watch a specific imported module package

        try:
            # HACK: manually simulate first resize/redraw
            curses.ungetch(curses.KEY_RESIZE)
        except curses.error:
            pass

        int2key: dict[int, str] = {
            getattr(curses, k): k for k in dir(curses) if k.startswith("KEY_")
        }

        prevlog = 0
        while True:
            ### FAIL: how to make !jurigged only reload on demand ?
            # from jurigged.register import registry
            # if registry.has_pending():
            #     registry.apply_pending()

            kpi("input")
            match wch := ui.input():
                # case curses.ERR:
                #     continue
                case "q":
                    break
                case _:
                    pass
            if isinstance(wch, int) and wch > ord(" ") and wch in int2key:
                wch = int2key[wch] + f"({wch})"
            k.log.info(f"{wch=}")

            kpi("bake")  # NOTE: overwrites prev values -> so keeps only last frame
            # THINK: ui.bake() to apply UI-specific constrains ?
            #   BUT: ui-model lives in .kernel, so .drv should be dumb
            #     ~~ unless "baking" inserts colorcodes
            va.wnd_w, va.wnd_h = ui.sizewh()
            # va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, max(1, va.wnd_h - 1))
            va.vp_w, va.vp_h = va.wnd_w, max(2, va.wnd_h - 1)
            displ = k.navi_sequence(nvid, va)

            kpi("draw")
            ui.clear()
            ui.draw_displ(displ)

            kpi("status")
            ## HACK: draw "status" *after* drawing evels -- to have all actual KPIs
            kpistr = " ".join(
                f"{nm}={(t1 - t0) / 1e6:.1f}ms"
                for (nm, t0), (_, t1) in pairwise(
                    ## DISABLED: order becomes wrong due to draw from prev frame
                    # sorted(g_dkpi.items(), key=lambda x: x[1])
                    ## FIXME: without sorting "bake" is negative
                    g_dkpi.items()
                )
            )
            kpistr = f"{wch!r} {kpistr} (tokens={len(displ)}) Nfd={process.num_fds()}"
            kpiw = min(va.wnd_w, width(kpistr))
            tok = TextSpan(0, 0, kpistr, kpiw, Aid.footer)
            if va.wnd_h > 0:
                ui.cursesdrv.draw_displ([tok._replace(y=va.wnd_h - 1)])
            ui.printdrv.draw_displ([tok])

            # FIXME: remove .printdrv depending on Print vs Curses
            #   IDEA! put ".tag/.tags=footer" into each token for semantic meaning
            #     >> then .printdrv in "worklog" mode can override y=0 for "footer" tag (to avoid gaps),
            #        and yet keep y=N-1 as-is in "screenful" mode to mimic curses
            endlog = len(k.log.ringbuffer)
            ui.printdrv.draw_lines(map(str, k.log.ringbuffer[prevlog:endlog]))
            ui.printdrv.draw_lines(["---\n"])
            prevlog = endlog

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
