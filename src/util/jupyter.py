import os
import sys
from typing import TYPE_CHECKING, Any, Final

from .logger import log

g_running_ipykernel: bool = False
g_running_ipyconsole: Any = None

CONNECTION_FILE: Final = "miur-ipython.json"


# WARN: no way to shutdown this kapp easily/cleanly
#   NEED: issue .shutdown() from client connection
#   OR: simulate that call by async def shutdown_request(...)
#     SRC: /usr/lib/python3.12/site-packages/ipykernel/kernelbase.py:985
def inject_ipykernel_into_asyncio(myloop: Any, myns: dict[str, Any]) -> None:
    global g_running_ipykernel
    if g_running_ipykernel:
        log.warning("ipykernel is already running! ignored")
        return

    # NOTE: make it easier to see ipython loading issues
    # FAIL: doesn't work together with stdio_to_altscreen()
    # with CE.curses_altscreen(stdscr):
    if sys.flags.isolated:
        __import__("site").main()  # lazy init for "site" in isolated mode
    # FIXED:DEPR: can be removed for !jupyter_core>=6
    os.environ["JUPYTER_PLATFORM_DIRS"] = "1"
    import ipykernel.kernelapp as IK

    os.environ["PYDEVD_DISABLE_FILE_VALIDATION"] = "1"  # =debugpy
    kapp: IK.IPKernelApp = IK.IPKernelApp.instance()
    assert not hasattr(kapp, "io_loop")
    kapp.connection_file = CONNECTION_FILE
    kapp.parent_handle = 1  # EXPL: suppress banner with conn details
    kapp.quiet = True  # EXPL: don't spam kapp's stdout/stderr with client's output
    # NOTE:BET: if I could run curses on different fd!=0/1 -- it would prevent so much workarounds! ※⡦⡬⣚⢓
    #   i.e. SCREEN *newterm(char *type, FILE *outfd, FILE *infd);
    kapp.capture_fd_output = (
        False  # EXPL: prevent jupyter from interfering with tty/curses fd=0/1
    )

    # EXPL:(outstream_class): prevent stdout/stderr redirection
    #   ALT: reset after .initialize() to see !pdb/etc. output
    #     kapp.reset_io()  # EXPL:FIXED: restore redirected stdout/sdterr
    # WKRND:DISABLED: allow redir of *default* stdout/stderr to client ※⡦⡕⢣⣗
    #   COS: code run in client is expected to print into client, NOT to kapp
    #   ALSO: send my logs to client inof altscreen (NICE: read w/o shell_out)
    #   NEED: all my log/print/stdout should use *explicit* FD from OPT
    # kapp.outstream_class = None  # type:ignore[assignment]

    log.warning(lambda: "bef kinit: %d" % __import__("threading").active_count())
    # OR:([]): ["python", "--debug"]
    kapp.initialize([])  # type:ignore[no-untyped-call]
    log.warning(lambda: "aft kinit: %d" % __import__("threading").active_count())

    ipyloop = IK.ioloop.IOLoop.current()  # type:ignore[attr-defined]
    assert ipyloop.asyncio_loop is myloop, "Bug: IPython doesn't use my own global loop"

    def _do_shutdown(self, restart):  # type:ignore[no-untyped-def]
        log.trace("jupyter: shutdown_request")
        self.shell_stream = None  # <HACK
        return {"status": "ok", "restart": restart}

    # HACK: monkey-patch kapp to prevent calling myloop.stop() on "shutdown_request"
    kapp.kernel.do_shutdown = _do_shutdown.__get__(kapp.kernel)

    # HACK: monkey-patch to have more control over messy IPython loop
    #   ALT:BAD: try: kapp.start(); finally: kio.close()
    old_start = ipyloop.start
    ipyloop.start = lambda: None  # EXPL: inof "kio.asyncio_loop.run_forever()"
    try:
        # NOTE: registers forever task=kernelbase.kapp.dispatch_queue()
        kapp.start()  # type:ignore[no-untyped-call]
    finally:
        ipyloop.start = old_start
    assert kapp.io_loop

    ns = kapp.shell.user_ns  # type:ignore[union-attr]
    ns["_entry"] = sys.modules["__main__"]
    ns["kapp"] = kapp
    ns.update(myns)
    log.trace("ipykernel had started")
    g_running_ipykernel = True


if TYPE_CHECKING:
    from collections.abc import Coroutine
    from typing import Optional


def ipyconsole_async(shutdown: bool = False) -> "Optional[Coroutine[Any, Any, Any]]":
    global g_running_ipyconsole  # pylint:disable=global-statement
    if g_running_ipyconsole:
        log.warning("ipyconsole is already running! ignored")
        return None

    if sys.flags.isolated:
        __import__("site").main()  # lazy init for "site" in isolated mode

    from jupyter_console.app import ZMQTerminalIPythonApp

    console: ZMQTerminalIPythonApp = ZMQTerminalIPythonApp.instance()
    console.existing = CONNECTION_FILE
    console.initialize([])  # CASE: .load_config_file()
    # pylint:disable=protected-access
    coro = console.shell._main_task()  # = app.start()
    if shutdown:
        console.shell.client.shutdown()
    g_running_ipyconsole = console
    return coro


def jupyter_client() -> None:
    import json

    # from jupyter_client.asynchronous.client import AsyncKernelClient
    from jupyter_client.client import KernelClient

    with open("confs/c1.json", "r") as f:
        data = json.load(f)
    shell_port = data["shell_port"]
    iopub_port = data["iopub_port"]
    stdin_port = data["stdin_port"]
    control_port = data["control_port"]
    hb_port = data["hb_port"]

    kc = KernelClient(
        ip="127.0.0.1",
        transport="tcp",
        shell_port=shell_port,
        iopub_port=iopub_port,
        stdin_port=stdin_port,
        control_port=control_port,
        hb_port=hb_port,
    )
    code = """import os
    current_dir = os.getcwd()
    print("Current working directory:", current_dir)"""
    msg_id = kc.execute(code)

    # OR:BET: jupyter-run = jupyter_client.runapp:RunApp.launch_instance
    # kc.shutdown(code)
    # sys.exit(0)
