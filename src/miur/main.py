import asyncio
import sys
import time
from argparse import ArgumentParser, Namespace
from collections.abc import Callable
from contextlib import ExitStack
from dataclasses import dataclass, field
from enum import IntEnum, auto
from types import TracebackType
from typing import Self

from . import log
from .kernel import MiurKernel, NaviId
from .systems.tuisystem import VisibleArea
from .uidrv.base_drv import BaseUIDriver


def cli_spec(parser: ArgumentParser) -> ArgumentParser:
    o = parser.add_argument
    _uiset = "pr printtext cu curses mu multi".split()
    o("--ui", choices=_uiset, default="multi")
    return parser


def cli_process() -> None:
    _ns = cli_spec(ArgumentParser()).parse_args(sys.argv[1:])


class LoopContext(Namespace):
    nvid: NaviId
    va: VisibleArea


# WTF: it seems we need per-client fn, as wnd size is different anyway
def process_frame(
    k: MiurKernel, ui: BaseUIDriver, ctx: LoopContext, wch: str | int
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
    ui.draw_displ(displ)

    log.measure("status")
    ## HACK: draw "status" *after* drawing evels -- to have all actual KPIs
    kpistr = f"{wch!r} {log.recent_measurements_avg()} (tokens={len(displ)}) "
    ui.draw_status(kpistr)

    if ui.__class__.__name__ == "PrintTextUIDriver":
        ui.draw_lines([log.archive_recent(dump=True), "---"])

    ui.refresh()
    log.measure("wait")
    return kpistr


class ComponentsManager:
    """Guarantee strict reverse-chronological teardown for togglable init groups"""

    def __init__(self) -> None:
        from collections import OrderedDict

        # BET? TypedDict to give actual type to possible keys like "curses"
        # RENAME? _cmpts | _togglables | _active[_resources|_registry]
        self._registry: OrderedDict[str, tuple[BaseUIDriver, ExitStack]] = OrderedDict()

    def __enter__(self) -> Self:
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> bool | None:  # OR: .close()
        ## ALT:BAD: we would need to chain exceptions to safely cleanup stuff
        ##   OR:TRY: inherit whole class from ExitStack to override behavior
        ##     COS:BAD:(too fragile): cb=._exit_callbacks[-1]  -->  ._exit_callbacks.remove(cb)
        ##   IDEA? pop all stacks into main_stack before __exit__
        # for name in list(reversed(self._registry.keys())):
        #     self.unregister(name)
        safe_stack = ExitStack()
        for _, cm in self._registry.values():
            safe_stack.enter_context(cm)
        return safe_stack.__exit__(exc_type, exc_val, exc_tb)

    def register(
        self, nm: str, factory: Callable[[str], tuple[BaseUIDriver, ExitStack]]
    ) -> BaseUIDriver:
        if nm in self._registry:
            # return self._registry[nm][0]
            raise RuntimeError(f"Component {nm=} is already registered")
        # The factory constructs the driver and attaches side-effects to the stack
        cmpt, cmpt_stack = factory(nm)
        # Store both the stack (for teardown) and the instance (for access)
        self._registry[nm] = (cmpt, cmpt_stack)
        return cmpt

    def unregister(self, nm: str) -> None:
        """Safely tears down a specific resource early by nm."""
        record = self._registry.pop(nm, None)
        if record is not None:
            _, stack = record
            stack.close()

    def __contains__(self, nm: str) -> bool:
        return nm in self._registry

    def __getitem__(self, nm: str) -> BaseUIDriver | None:
        if record := self._registry.get(nm):
            return record[0]
        return None


class ArchCmd(IntEnum):
    KERNEL_SHUTDOWN = auto()
    # CHG: {cmd:enable/disable, arg:multi_drv}
    #   OR: topo.multi_drv.enable(0/1)
    #   OR: {grp:UIDrv, arg:Curses, cmd:enable, val:1}
    UIDRV_TOGGLE = auto()
    DISPLAYLIST = auto()


@dataclass(frozen=True, slots=True)
class Event:
    # kind: IntEnum = "ArchCmd"
    cmd: ArchCmd
    arg: str = ""
    val: int = 1
    key: str = ""
    ts: int = field(default_factory=time.monotonic_ns)
    # requester = "kernel"
    # rpc_ctx = "kernel"
    # report_to = "client-1"


# RENAME? MiurKernel (COS: what I currently call "kernel" is just a lib...)
#   FIND: should "kernel" in foss be a daemon/process (i.e. have loop), or just a lib?
class MiurServer:
    _stack: ExitStack
    _cmgr: ComponentsManager

    def __init__(self, k: MiurKernel, ctx: LoopContext) -> None:
        self.k = k
        self.ctx = ctx
        # FUT:OPT: pyzmq
        #   * ZeroMQ as universal bridge to C++/Rust/Go clients
        #   ~ BUT! we still need plain pipe/socket readers to connect to miur from bash or netcat,
        #     and it's better to keep stream format/behavior exactly the same over all clients
        #     ~~ so, basically no point in having zmq yet
        # FAIL: asyncio.Queue is NOT thread-safe for this intended purpose
        #   BET? queue.SimpleQueue
        self.eventq: asyncio.Queue[Event] = asyncio.Queue()
        self.dispatcher_task: asyncio.Task[None] | None = None

    def __enter__(self) -> Self:
        with ExitStack() as stack:
            self._cmgr = stack.enter_context(ComponentsManager())
            self._stack = stack.pop_all()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> bool | None:
        return self._stack.__exit__(exc_type, exc, tb)

    def emit(self, ev: Event) -> None:
        # TEMP: prefer to error-out when queue is full
        #   >> decide what to do with it later, as "waiting notfull" isn't good for UI
        self.eventq.put_nowait(ev)

    def handle_unified_input(self, key: str) -> None:
        log.warning(f"{key=}")
        match key:
            # case str() and "0" <= char <= "9":
            # case "A" | "B" | "C":
            # case ["move", str(direction), int(distance)]:
            # case str() if key.isalnum():
            case "q":
                self.emit(Event(ArchCmd.KERNEL_SHUTDOWN))
                ## ALT:THINK:ARCH-vs-PERF:BET! directly process here for lower latency
                ##   BUT:BAD: we are missing central_dispatcher/queue with multi-hooks
                ##     ALT:IDEA: re-insert synthetic events with "already_processed" flag
                ##       BAD: hooks won't be able to remove/transform events before their execution
                # self.process_event(Event(...))
            case "j":
                log("V", "down")
            case _:
                pass
        # self.key: str | int = "startup" = key
        self.emit(
            Event(ArchCmd.DISPLAYLIST, "REGEN", val=1, key=key)
        )  # <TEMP:MOVE: after processing cmd itself

    # FIXME: translate into universal key-aliases first, before mapping to Miur*Cmd
    def handle_input(self, nm: str) -> None:
        log.measure("input")
        ui = self._cmgr[nm]
        wch = ui.input()
        self.handle_unified_input(wch)

    async def central_dispatcher(self) -> None:
        while True:
            ev = await self.eventq.get()  # TBD: except QueueShutDown
            try:
                # USAGE:(ui keypress): loop.call_soon_threadsafe(self.eventq.put_nowait, Event(...))
                self.process_event(ev)
            except Exception as exc:
                # from rich.console import Console
                # console = Console()
                # console.print_exception(show_locals=True)
                log.error(exc)
                try:
                    # BUT:WHY? when printing to not-crashed curses we also got ERR
                    #   _curses.error: addwstr() returned ERR
                    if ui := self._cmgr["printtext"]:
                        ui.draw_lines([log.archive_recent(dump=True)])
                except Exception:
                    pass
            finally:
                self.eventq.task_done()

    async def start(self) -> None:
        init_cfg = [
            Event(ArchCmd.UIDRV_TOGGLE, "printtext", 1),
            Event(ArchCmd.UIDRV_TOGGLE, "curses", 1),
            Event(ArchCmd.DISPLAYLIST, "REGEN", 1, key="startup"),
            # Event(cmd=ArchCmd.KERNEL_SHUTDOWN),  # <PERF:DEBUG:(immediate shutdown)
        ]
        self.dispatcher_task = asyncio.create_task(self.central_dispatcher())
        for ev in init_cfg:
            self.emit(ev)
        try:
            await self.dispatcher_task
        except asyncio.CancelledError:
            # WARN:CHECK: may CancelledError be raised here on NOT clean exit?
            pass
        # finally:
        #     # 5. Clean up any transports left open if the engine exits abruptly
        #     for transport in list(self.transports.values()):
        #         await transport.stop()

    def process_event(self, ev: Event) -> None:
        match ev:
            case Event(cmd=ArchCmd.KERNEL_SHUTDOWN):
                log.warning("Shutdown signal received. Stopping Core Dispatcher...")
                if self.dispatcher_task:
                    self.dispatcher_task.cancel()
            case Event(cmd=ArchCmd.UIDRV_TOGGLE, arg=arg, val=val):
                self.toggle_cmpt(arg, enable=val)
            case Event(cmd=ArchCmd.DISPLAYLIST, arg="REGEN", val=val, key=key):
                # FIXME? instead of redrawing on each event -- simply invalidate flag
                #   and then trigger redraw after grace period?
                #   >> at least collapse all such events queued and react only on last one
                if val > 0:
                    # TODO: opt arg to DISPLAYLIST to update only listed affected UI
                    if ui := self._cmgr["curses"]:
                        _kpistr = process_frame(self.k, ui, self.ctx, wch=key)
                    if ui := self._cmgr["printtext"]:
                        try:
                            _kpistr = process_frame(self.k, ui, self.ctx, wch=key)
                        except OSError as exc:
                            import errno

                            if exc.errno == errno.EIO:
                                log.warning("HYPO: term died")
                                self._cmgr.unregister("printtext")
                                # NOTE: redraw other windows, as they most likely resized
                                #   FAIL? doesn't work, maybe need SIGWINCH handler ?
                                # self.emit(Event(ArchCmd.DISPLAYLIST, "REGEN", val=1, key="crash"))
                            else:
                                raise

            case _:
                raise NotImplementedError(ev)

    # RENAME? make_cmpt  FIND: should "Factory" be always Type/Callable, or can be instance?
    # MOVE?/SPLIT? factory into e.g. -> Curses*UIAdapter (plus .handle_input*)
    def cmpt_factory(self, nm: str) -> tuple[BaseUIDriver, ExitStack]:
        loop = asyncio.get_running_loop()
        with ExitStack() as cmpt_stack:
            do = cmpt_stack.enter_context
            __ = cmpt_stack.callback
            match nm:
                case "mu" | "multi":
                    from .uidrv.multi_drv import MultiUIDriver

                    ui = do(MultiUIDriver())

                case "cu" | "curses":
                    from .uidrv.curses_drv import CursesUIDriver

                    ui = do(CursesUIDriver())

                    # loop.add_signal_handler(signal.SIGWINCH, g.curses_ui.resize)
                    # loop.add_reader(iomgr.CURSES_STDIN_FD, g.curses_ui.handle_input)
                    CURSES_STDIN_FD = sys.stdin.fileno()
                    loop.add_reader(
                        CURSES_STDIN_FD, lambda: self.handle_input("curses")
                    )
                    __(lambda: loop.remove_reader(CURSES_STDIN_FD))

                case "pr" | "printtext":
                    from .uidrv.connectors.termpipe import new_termwindow
                    from .uidrv.printtext_drv import PrintTextUIDriver

                    rtty, wtty = do(new_termwindow())
                    ui = do(PrintTextUIDriver(rtty, wtty))

                    rfd = rtty.fileno()
                    loop.add_reader(rfd, lambda: self.handle_input("printtext"))
                    __(lambda: loop.remove_reader(rfd))

                case _:
                    raise ValueError(nm)
            return (ui, cmpt_stack.pop_all())

    # MAYBE? ,factory: Callable[[], AbstractContextManager[BaseUIDriver]],
    def toggle_cmpt(self, nm: str, enable: int | bool | None = None) -> None:
        present = nm in self._cmgr
        if isinstance(enable, int):
            enable = bool(enable) if enable >= 0 else None
        if enable is None:
            enable = not present
        elif enable == present:
            return
        # if enable != bool(factory):
        #     raise ValueError(factory)
        if enable:
            self._cmgr.register(nm, self.cmpt_factory)
        else:
            self._cmgr.unregister(nm)


def main_navi(stack: ExitStack) -> None:  # noqa: PLR0915  # pylint:disable=too-many-locals,too-many-statements
    do = stack.enter_context
    log.kpi("enter(navi)")

    from .dev.excepthook import set_excepthook

    do(set_excepthook())

    from .dev.warnings import enable_warnings

    enable_warnings()
    log.kpi("after(warnings)")

    # from .dev.hotreload import enable_jurigged
    #
    # enable_jurigged()
    # log.kpi("after(jurigged)")

    from .dev.tracecode import enable_tracelines

    # enable_tracelines()
    log.kpi("after(tracelines)")

    k = MiurKernel()
    ctx = LoopContext()
    # h = "/data/g/miur_gen/demo/errors/chained.py"
    ctx.nvid = k.new_navi(0, "/etc")
    ctx.va = VisibleArea(8, 11)  # OR? use "vpid"
    log.kpi("after(ui_drv)")

    log.kpi("before(asyncio)")
    with MiurServer(k, ctx) as srv:
        # FIND: can we combine "async with MiurServer()" with .run() ?
        asyncio.run(srv.start(), debug=True)
    # BET: use lightweight tracer: sys.setprofile(log.profile)
    log.kpi("return(navi)")


def set_prname(appname: str) -> None:
    # FIXED:USAGE: $ pkill miur
    # ALT: https://pypi.org/project/setproctitle/
    if sys.platform == "linux":
        import ctypes

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

        from .dev.exitstack import enable_erasure_guardians

        enable_erasure_guardians()

        with ExitStack() as stack:
            main_navi(stack)
            rc = 2  # <CASE: successful init but failing deinit
    except Exception as exc:
        log.error(exc)
        ## DISABLED:PERF:BAD: +400ms
        # from rich.traceback import install
        # install(show_locals=True)
        # raise
    else:
        rc = 0
    log.kpi(f"return(main) -> {rc}")
    print(log.archive_recent(dump=True), file=sys.stderr)
    ## DISABLED: should only be used for jurigged+devloop
    ##   << orse results in exitcode!=0
    # if "jurigged" in __import__("sys").modules:
    #     return _kpistr
    return rc
