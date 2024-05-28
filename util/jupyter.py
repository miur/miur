import os
import sys
from typing import Any

from .logger import log

g_running_ipykernel: bool = False


def inject_ipykernel_into_asyncio(myns: dict[str, Any]) -> None:
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
    assert not hasattr(kernel, 'io_loop')
    kernel.connection_file = "miur-ipython.json"
    kernel.parent_handle = 1  # EXPL: suppress banner with conn details
    # EXPL:(outstream_class): prevent stdout/stderr redirection
    #   ALT: reset after .initialize() to see !pdb/etc. output
    #     kernel.reset_io()  # EXPL:FIXED: restore redirected stdout/sdterr
    # [_] MAYBE: allow at least TTY stdout/stderr redir to client ?
    #   COS: read logs in client w/o printing to altscreen and then shell_out to read them
    kernel.outstream_class = None  # type:ignore

    log.warning(lambda: "bef kinit: %d" % __import__("threading").active_count())
    kernel.initialize([])  # type:ignore  # OR:([]): ["python", "--debug"]
    log.warning(lambda: "aft kinit: %d" % __import__("threading").active_count())

    ipyloop = IK.ioloop.IOLoop.current()  # type:ignore
    assert (
        ipyloop.asyncio_loop is __import__("asyncio").get_running_loop()
    ), "Bug: IPython doesn't use my own global loop"

    # HACK: monkey-patch to have more control over messy IPython loop
    #   ALT:BAD: try: kernel.start(); finally: kio.close()
    old_start = ipyloop.start
    ipyloop.start = lambda: None  # EXPL: inof "kio.asyncio_loop.run_forever()"
    try:
        kernel.start()  # type:ignore
    finally:
        ipyloop.start = old_start
    assert kernel.io_loop

    ns = kernel.shell.user_ns  # type:ignore
    ns["_entry"] = sys.modules["__main__"]
    ns["ipk"] = kernel
    ns.update(myns)
    log.trace("ipykernel had started")
    g_running_ipykernel = True


if None:
    if opts.jupyter_kernel:
        from ipykernel.kernelapp import launch_new_instance
        from traitlets.config import Config

        c = Config()

        # ipykernel 6.x breaks redirecting stdout · Issue #795 · ipython/ipykernel · GitHub ⌇⡤⢆⡠⡨
        #   https://github.com/ipython/ipykernel/issues/795
        # c.IPKernelApp.capture_fd_output = False

        ## FIXED: don't show "how to exit" msg (i.e. connect client and issue "quit") ⌇⡤⢆⡧⣧
        #   https://ipython.readthedocs.io/en/stable/config/options/kernel.html#configtrait-IPKernelApp.parent_handle
        # c.IPKernelApp.parent_handle = 1

        ## [_] TODO: integrate my main loop epoll/signals with asyncio from IPython
        # c.InteractiveShellApp.gui = 'asyncio'

        ## FIXED: single known file -- to avoid manually copying its name each time
        c.ConnectionFileMixin.connection_file = "miur-kernel.json"

        launch_new_instance(config=c)
        sys.exit(0)

    if opts.jupyter_console:
        pass

    if opts.jupyter_client:
        import json

        # from jupyter_client.asynchronous.client import AsyncKernelClient
        from jupyter_client.client import KernelClient

        json_file = open("confs/c1.json", "r")
        data = json.load(json_file)
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
        sys.exit(0)

    if opts.jupyter_quit:
        # OR:BET: jupyter-run = jupyter_client.runapp:RunApp.launch_instance
        kc.shutdown(code)
        sys.exit(0)
