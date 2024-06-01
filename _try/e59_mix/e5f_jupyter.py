# DEBUG: $ PYDEVD_DISABLE_FILE_VALIDATION=1 python -m pdb e5f_jupyter.py
"""An in-process terminal example."""

import asyncio
from signal import SIGINT

from ipykernel.kernelapp import IPKernelApp, ioloop

# from IPython.lib.kernel import connect_qtconsole
# from ipykernel.inprocess.manager import InProcessKernelManager
## from ipykernel.inprocess.ipkernel import InProcessInteractiveShell
# from jupyter_console.ptshell import ZMQTerminalInteractiveShell
## ZMQTerminalIPythonApp


def main() -> None:
    """The main function."""

    kernel = IPKernelApp.instance()
    kernel.initialize(
        [
            "python",
            "--IPKernelApp.parent_handle=1",
            # '--debug',
            # -f ipython-kernel.json
            # "--IPKernelApp.outstream_class=",
            # "--IPKernelApp.displayhook_class=",
        ]
    )
    ## FIXED: restore redirected stdout/sdterr for !pdb
    #   print(kernel)  # FAIL: internally redirected IO
    #   print(kernel, file=__import__("sys").__stderr__)  # WKRND
    kernel.reset_io()

    ## FIXED: restore ignored SIGINT
    # BAD: we can't raise exceptions from inside native sig handlers
    #   signal.signal(signal.SIGINT, lambda si,fr: None)
    # FAIL: no running event loop
    #   loop = asyncio.get_running_loop()
    def setup_handler():
        loop = asyncio.get_running_loop()
        loop.add_signal_handler(SIGINT, lambda: asyncio.get_running_loop().stop())

    # NOTE: this is only possible, because IPython already started IOLoop in bkgr Thread
    ioloop.IOLoop.current().add_callback(setup_handler)

    # breakpoint()  # import pdb; pdb.run('kernel.start()')
    kernel.start()

    #####
    # console = connect_qtconsole(kernel.abs_connection_file, profile=kernel.profile)
    # kernel.shell.user_ns['kernel'] = kernel

    # app.quit()
    # console.kill()
    # kernel.io_loop.stop()

    # kernel_manager = InProcessKernelManager()
    # kernel_manager.start_kernel()
    #
    # print(kernel_manager.get_connection_info(session=True))
    # # {'transport': 'tcp', 'ip': '127.0.0.1', 'shell_port': 0, 'iopub_port': 0, 'stdin_port': 0,
    # #  'hb_port': 0, 'control_port': 0, 'session': <jupyter_client.session.Session object at 0x784f616b9a60>}
    # print(kernel_manager.connection_file)
    # # 9c56d914-b48294caf462ead750390951_44685_1
    #
    # kernel = kernel_manager.kernel
    # # kernel.gui = "qt4"
    # kernel.shell.push({"foo": 43})
    #
    # client = kernel_manager.client()
    # client.start_channels()
    # client.execute('bar = 5')
    # print(client.execute('bar'))
    #
    # shell = ZMQTerminalInteractiveShell(manager=kernel_manager, client=client)
    # shell.mainloop()


if __name__ == "__main__":
    main()


def _integ_with_asyncio():
    pass
    ## FIXED: restore ignored SIGINT
    # BAD: we can't raise exceptions from inside native sig handlers
    #   signal.signal(signal.SIGINT, lambda si,fr: None)
    # FAIL: no running event loop
    #   loop = asyncio.get_running_loop()
    # tasks = []
    # def setup_mainapp() -> None:
    #     loop = asyncio.get_running_loop()
    #     # kio = kernel.io_loop
    #     # loop.add_signal_handler(signal.SIGINT, lambda: kio.add_callback(kio.stop))
    #     tasks.append(loop.create_task(run_mainapp(stdscr), name='main_app'))
    # kio.add_callback(setup_mainapp)
    #
    # async def run_mainapp(stdscr: C.window) -> None:
    #     await mainloop_asyncio(stdscr)
    #     kernel.io_loop.stop()
    #     ## OR: together
    #     # async with asyncio.TaskGroup() as tg:
    #     #     _t1 = tg.create_task(mainloop_asyncio(stdscr))
    #     #     _t2 = tg.create_task(ipykernel(...))
    # # tasks.append(ipyloop.create_task(run_mainapp(stdscr), name="main_app"))


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
