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
