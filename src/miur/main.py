import asyncio
import ctypes
import curses as C
import gc
import os
import signal
import sys
from argparse import ArgumentParser, Namespace

# pyright: reportMissingTypeStubs=false
import jurigged
import psutil
from jurigged.codetools import UpdateOperation
from wcwidth import width

from . import log
from .kernel import MiurKernel, NaviId
from .systems.tuisystem import VisibleArea
from .uicommon.displaylist import TextSpan
from .uicommon.styleids import Aid
from .uidrv.multi_drv import MultiUIDriver

gc.set_debug(gc.DEBUG_UNCOLLECTABLE)


if not sys.warnoptions:
    # WHY: print a traceback pinpointing exactly where the unclosed file object was created.
    __import__("warnings").simplefilter("always", ResourceWarning)


def cli_spec(parser: ArgumentParser) -> ArgumentParser:
    o = parser.add_argument
    _uiset = "pr printtext cu curses mu multi".split()
    o("--ui", choices=_uiset, default="multi")
    return parser


# def cli_process() -> object:
#     ns = cli_spec(ArgumentParser()).parse_args(sys.argv[1:])
#     match ns.ui:
#         case "mu" | "multi":
#             from .uidrv.multi_drv import MultiUIDriver
#
#             UIDrv = MultiUIDriver
#         case "cu" | "curses":
#             from .uidrv.curses_drv import CursesUIDriver
#
#             UIDrv = CursesUIDriver
#         case "pr" | "printtext":
#             from .uidrv.printtext_drv import PrintTextUIDriver
#
#             UIDrv = PrintTextUIDriver
#         case _:
#             from typing import assert_never
#
#             assert_never(ns.ui)
#     return UIDrv


class LoopContext(Namespace):
    process: psutil.Process
    nvid: NaviId
    va: VisibleArea
    int2key: dict[int, str]


def process_frame(
    k: MiurKernel, ui: MultiUIDriver, ctx: LoopContext, wch: str | int
) -> str:
    log.measure("bake")  # NOTE: overwrites prev values -> so keeps only last frame
    # THINK: ui.bake() to apply UI-specific constrains ?
    #   BUT: ui-model lives in .kernel, so .drv should be dumb
    #     ~~ unless "baking" inserts colorcodes
    va = ctx.va
    va.wnd_w, va.wnd_h = ui.sizewh()
    # va.vp_w, va.vp_h = min(100, va.wnd_w), min(7, max(1, va.wnd_h - 1))
    va.vp_w, va.vp_h = va.wnd_w, max(2, va.wnd_h - 1)
    displ = k.navi_sequence(ctx.nvid, va)

    log.measure("draw")
    ui.clear()
    ui.printdrv.draw_lines(["\n"])
    ui.draw_displ(displ)
    # log.info(ui.printdrv.style_by_id)

    log.measure("status")
    ## HACK: draw "status" *after* drawing evels -- to have all actual KPIs
    kpistr = f"{wch!r} {log.recent_measurements_avg()} (tokens={len(displ)}) "
    kpiw = min(va.wnd_w, width(kpistr))
    tok = TextSpan(0, 0, kpistr, kpiw, Aid.FOOTER)
    if va.wnd_h > 0:
        ui.cursesdrv.draw_displ([tok._replace(y=va.wnd_h - 1)])
    ui.printdrv.draw_displ([tok])

    # FIXME: remove .printdrv depending on Print vs Curses
    #   IDEA! put ".tag/.tags=footer" into each token for semantic meaning
    #     >> then .printdrv in "worklog" mode can override y=0 for "footer" tag (to avoid gaps),
    #        and yet keep y=N-1 as-is in "screenful" mode to mimic curses
    ui.printdrv.draw_lines([log.archive_recent(dump=True), "---"])

    ui.refresh()
    log.measure("wait")
    return kpistr


def main_navi() -> None:  # noqa: PLR0915  # pylint:disable=too-many-locals,too-many-statements
    log.kpi("enter(navi)")
    k = MiurKernel()
    ctx = LoopContext()
    ctx.process = psutil.Process(os.getpid())
    # h = "/data/g/miur_gen/demo/errors/chained.py"
    ctx.nvid = k.new_navi(0, "/etc")
    ctx.va = VisibleArea(8, 11)  # OR? use "vpid"

    with MultiUIDriver() as ui:
        log.kpi("after(ui_drv)")
        ## BAD: jurigged produces multiple logs per single saved file --> screen refreshes N-times
        ##   TODO: spawn/reset 500ms timer after each event, and only refresh when timer is done

        ## FAIL: when curses is disabled -- no one injects KEY_RESIZE event into getch() to redraw loop
        ##   and we can't even raise exception from here, risking to break python code
        # signal.signal(signal.SIGWINCH, lambda si, fr: ui.refresh())

        def jurigged_on_event(event: object) -> None:
            log.verbose(event)
            # OR: if "Evaluating" in str(event) or "Update" in str(event): pass; else: return
            if isinstance(event, UpdateOperation):
                ## DISABLED:FAIL: doesn't unblock already waiting getch()
                # try:
                #     # HACK: unblock current .getch()
                #     # C.ungetch(C.KEY_REFRESH)
                #     # C.ungetch(C.KEY_RESIZE)  # safe
                #     # OR:(^L=12): ungetch(12) | ungetch(C.KEY_F5)
                # except C.error:
                #     pass
                ## FIXED:WKRND: Send a native signal to wake up the main thread's getch()
                os.kill(os.getpid(), signal.SIGWINCH)

        ### HACK: hot-reload (recursive ./*.py files from PWD?)
        ## CHECK: if it has import-hook to discover lazily loaded modules later
        # OR: jurigged.watch(pattern=[fs.dirname(fs.realpath(__file__)) + "/**/*.py"], logger=jurigged_on_event)
        jurigged.watch(logger=jurigged_on_event)  # pyright: ignore[reportUnknownMemberType]  # <CASE: recursive
        # jurigged.watch("miur") # <OR watch a specific package directory (non-recursive)
        # import mymod; jurigged.watch(mymod) # <OR watch a specific imported module package
        log.kpi("after(jurigged)")

        ## DISABLED:FAIL: I would need to send this to *EACH* driver in turn
        ##   HACK: manually simulate first resize/redraw
        # try: C.ungetch(C.KEY_RESIZE); except C.error: pass

        ctx.int2key = {getattr(C, k): k for k in dir(C) if k.startswith("KEY_")}

        wch: str | int = "startup"
        while True:
            ### FAIL: how to make !jurigged only reload on demand ?
            # from jurigged.register import registry
            # if registry.has_pending():
            #     registry.apply_pending()
            try:
                _kpistr = process_frame(k, ui, ctx, wch)
                log.measure("input")
                wch = ui.input()
                if isinstance(wch, int) and (nm := ctx.int2key.get(wch, "")):
                    wch = nm + f"({wch})"
                log.warning(f"{wch=}")
                match wch:
                    # case C.ERR:
                    #     continue
                    case "q":
                        break
                    case "j":
                        log("V", "down")
                    case _:
                        pass

            except Exception as exc:
                # from rich.console import Console
                # console = Console()
                # console.print_exception(show_locals=True)
                log.error(exc)
                ui.printdrv.draw_lines([log.archive_recent(dump=True)])

    log.kpi("before(asyncio)")
    asyncio.run(mainloop_asyncio(), debug=True)
    log.kpi("return(navi)")


def set_prname(appname: str) -> None:
    # FIXED:USAGE: $ pkill miur
    # ALT: https://pypi.org/project/setproctitle/
    if sys.platform == "linux":
        maxcommlen = 15
        assert len(appname) <= maxcommlen, "limit for /proc/PID/comm"
        libc = ctypes.CDLL("libc.so.6")
        PR_SET_NAME = 15
        libc.prctl(PR_SET_NAME, appname.encode("utf-8"), 0, 0, 0)


def main() -> int:
    log.kpi("enter(main)")
    rc = 1
    try:
        set_prname("miur")
        main_navi()
        rc = 0
    except Exception as exc:
        log.error(exc)
        ## DISABLED:PERF:BAD: +400ms
        # from rich.traceback import install
        # install(show_locals=True)
        # raise
    log.kpi(f"return(main) -> {rc}")
    print(log.archive_recent(dump=True), file=sys.stderr)
    ## DISABLED: should only be used for jurigged+devloop
    ##   << orse results in exitcode!=0
    # if "jurigged" in __import__("sys").modules:
    #     return _kpistr
    return rc
