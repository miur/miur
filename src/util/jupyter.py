import os
import sys
from typing import TYPE_CHECKING, Any, Final

from .logger import log

g_running_ipykernel: bool = False
g_running_ipyconsole: bool = False

CONNECTION_FILE: Final = "miur-ipython.json"


# WARN: no way to shutdown this kernel easily/cleanly
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
    kernel: IK.IPKernelApp = IK.IPKernelApp.instance()
    assert not hasattr(kernel, "io_loop")
    kernel.connection_file = CONNECTION_FILE
    kernel.parent_handle = 1  # EXPL: suppress banner with conn details
    kernel.quiet = True  # EXPL: don't spam kernel's stdout/stderr with client's output

    # EXPL:(outstream_class): prevent stdout/stderr redirection
    #   ALT: reset after .initialize() to see !pdb/etc. output
    #     kernel.reset_io()  # EXPL:FIXED: restore redirected stdout/sdterr
    # WKRND:DISABLED: allow redir of *default* stdout/stderr to client ※⡦⡕⢣⣗
    #   COS: code run in client is expected to print into client, NOT to kernel
    #   ALSO: send my logs to client inof altscreen (NICE: read w/o shell_out)
    #   NEED: all my log/print/stdout should use *explicit* FD from OPT
    # kernel.outstream_class = None  # type:ignore[assignment]

    log.warning(lambda: "bef kinit: %d" % __import__("threading").active_count())
    # OR:([]): ["python", "--debug"]
    kernel.initialize([])  # type:ignore[no-untyped-call]
    log.warning(lambda: "aft kinit: %d" % __import__("threading").active_count())

    ipyloop = IK.ioloop.IOLoop.current()  # type:ignore[attr-defined]
    assert ipyloop.asyncio_loop is myloop, "Bug: IPython doesn't use my own global loop"

    # HACK: monkey-patch to have more control over messy IPython loop
    #   ALT:BAD: try: kernel.start(); finally: kio.close()
    old_start = ipyloop.start
    ipyloop.start = lambda: None  # EXPL: inof "kio.asyncio_loop.run_forever()"
    try:
        # NOTE: registers forever task=kernelbase.Kernel.dispatch_queue()
        kernel.start()  # type:ignore[no-untyped-call]
    finally:
        ipyloop.start = old_start
    assert kernel.io_loop

    ns = kernel.shell.user_ns  # type:ignore[union-attr]
    ns["_entry"] = sys.modules["__main__"]
    ns["ipk"] = kernel
    ns.update(myns)
    log.trace("ipykernel had started")
    g_running_ipykernel = True


if TYPE_CHECKING:
    from collections.abc import Coroutine


def ipyconsole_async() -> "Coroutine[] | None":
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
    coro = console.shell._main_task()  # = app.start()
    g_running_ipyconsole = True
    return coro
    # console.shell.shutdown()