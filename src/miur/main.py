import asyncio
import ctypes
import curses as C
import gc
import os
import signal
import sys
from argparse import ArgumentParser, Namespace
from contextlib import AsyncExitStack, ExitStack
from typing import NamedTuple, assert_never

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


def enable_jurigged() -> None:
    ## BAD: jurigged produces multiple logs per single saved file --> screen refreshes N-times
    ##   TODO: spawn/reset 500ms timer after each event, and only refresh when timer is done

    ## FAIL: when curses is disabled -- no one injects KEY_RESIZE event into getch() to redraw loop
    ##   and we can't even raise exception from here, risking to break python code
    # signal.signal(signal.SIGWINCH, lambda si, fr: ui.refresh())

    import jurigged  # pyright: ignore[reportMissingTypeStubs]

    def jurigged_on_event(event: object) -> None:
        log.verbose(event)
        # OR: if "Evaluating" in str(event) or "Update" in str(event): pass; else: return
        if str(event).startswith("Update "):
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

    ### FAIL: how to make !jurigged only reload on demand ?
    # from jurigged.register import registry
    # while True: ... if registry.has_pending(): registry.apply_pending()


class LoopContext(Namespace):
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


def main_navi(stack: ExitStack) -> None:  # noqa: PLR0915  # pylint:disable=too-many-locals,too-many-statements
    do = stack.enter_context
    log.kpi("enter(navi)")
    k = MiurKernel()
    ctx = LoopContext()
    # h = "/data/g/miur_gen/demo/errors/chained.py"
    ctx.nvid = k.new_navi(0, "/etc")
    ctx.va = VisibleArea(8, 11)  # OR? use "vpid"

    ui = do(MultiUIDriver())
    ctx.int2key = {getattr(C, k): k for k in dir(C) if k.startswith("KEY_")}

    log.kpi("after(ui_drv)")
    enable_jurigged()
    log.kpi("after(jurigged)")

    wch: str | int = "startup"
    while True:
        try:
            _kpistr = process_frame(k, ui, ctx, wch)

            ## DISABLED:FAIL: I would need to send this to *EACH* driver in turn
            ##   HACK: manually simulate first resize/redraw
            # try: C.ungetch(C.KEY_RESIZE); except C.error: pass
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


from enum import IntEnum, auto


class ArchCmd(IntEnum):
    KERNEL_SHUTDOWN = auto()
    MULTI_DRV_ENABLE = auto()
    CURSES_DRV_ENABLE = auto()


class Event(NamedTuple):
    # kind: IntEnum
    cmd: ArchCmd
    # arg: int = 1
    # requester = "kernel"
    # rpc_ctx = "kernel"
    # report_to = "client-1"


class MiurApp:
    def __init__(self) -> None:
        # FUT:OPT: pyzmq
        #   * ZeroMQ as universal bridge to C++/Rust/Go clients
        #   ~ BUT! we still need plain pipe/socket readers to connect to miur from bash or netcat,
        #     and it's better to keep stream format/behavior exactly the same over all clients
        #     ~~ so, basically no point in having zmq yet
        # FAIL: asyncio.Queue is NOT thread-safe for this intended purpose
        #   BET? queue.SimpleQueue
        self.eventq: asyncio.Queue[Event] = asyncio.Queue()
        self.dispatcher_task: asyncio.Task | None = None

    async def central_dispatcher(self) -> None:
        try:
            while True:
                ev = await self.eventq.get()
                try:
                    # When the UI thread catches a keypress, it safely schedules it into the loop:
                    # loop.call_soon_threadsafe(event_queue.put_nowait, clicked_event)
                    self.process_event(ev)
                except Exception as exc:
                    print(f"Pipeline error processing event: {exc}", file=sys.stderr)
                finally:
                    self.eventq.task_done()
        except asyncio.CancelledError:
            print("Central dispatcher closing down.")
            raise

    async def start(self) -> None:
        async with AsyncExitStack() as stack:
            # loop = asyncio.get_running_loop()
            # loop.add_signal_handler(signal.SIGWINCH, g.curses_ui.resize)
            # loop.add_reader(iomgr.CURSES_STDIN_FD, g.curses_ui.handle_input)
            self.dispatcher_task = asyncio.create_task(self.central_dispatcher())
            # 2. Inject an admin command onto the queue to activate the initial transport cleanly
            await self.eventq.put(ArchCmd.MULTI_DRV_ENABLE)
        # 4. Block on the dispatcher's execution lifecycle
        try:
            await self.dispatcher_task
        except asyncio.CancelledError:
            pass

    async def process_event(self, ev: Event) -> None:
        match ev.cmd:
            case ArchCmd.KERNEL_SHUTDOWN:
                log.warning("Shutdown signal received. Stopping Core Dispatcher...")
                if self.dispatcher_task:
                    self.dispatcher_task.cancel()
            case ArchCmd.MULTI_DRV_ENABLE:  # REMOVE multi and spawn each individually
                raise NotImplementedError
            case _:
                assert_never(ev.cmd)


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
        with ExitStack() as stack:
            main_navi(stack)
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
