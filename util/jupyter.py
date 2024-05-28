import os
import sys
from typing import Any

from .logger import log

g_running_ipykernel: bool = False


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
    kernel = IK.IPKernelApp.instance()
    assert not hasattr(kernel, "io_loop")
    kernel.connection_file = "miur-ipython.json"
    kernel.parent_handle = 1  # EXPL: suppress banner with conn details

    # EXPL:(outstream_class): prevent stdout/stderr redirection
    #   ALT: reset after .initialize() to see !pdb/etc. output
    #     kernel.reset_io()  # EXPL:FIXED: restore redirected stdout/sdterr
    # WKRND:DISABLED: allow redir of *default* stdout/stderr to client ※⡦⡕⢣⣗
    #   COS: code run in client is expected to print into client, NOT to kernel
    #   ALSO: send my logs to client inof altscreen (NICE: read w/o shell_out)
    #   NEED: all my log/print/stdout should use *explicit* FD from OPT
    # kernel.outstream_class = None  # type:ignore[assignment]

    log.warning(lambda: "bef kinit: %d" % __import__("threading").active_count())
    kernel.initialize([])  # type:ignore  # OR:([]): ["python", "--debug"]
    log.warning(lambda: "aft kinit: %d" % __import__("threading").active_count())

    ipyloop = IK.ioloop.IOLoop.current()  # type:ignore
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

    ns = kernel.shell.user_ns  # type:ignore
    ns["_entry"] = sys.modules["__main__"]
    ns["ipk"] = kernel
    ns.update(myns)
    log.trace("ipykernel had started")
    g_running_ipykernel = True


def ipyconsole_out() -> None:
    pass
