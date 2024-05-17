"""An in-process terminal example."""

# import asyncio

# from ipykernel.inprocess.manager import InProcessKernelManager
## from ipykernel.inprocess.ipkernel import InProcessInteractiveShell
# from jupyter_console.ptshell import ZMQTerminalInteractiveShell
## ZMQTerminalIPythonApp

# from IPython.lib.kernel import connect_qtconsole
from ipykernel.kernelapp import IPKernelApp

def main() -> None:
    """The main function."""

    kernel = IPKernelApp.instance()
    kernel.initialize(['python'])
    kernel.start()

    # console = connect_qtconsole(kernel.abs_connection_file, profile=kernel.profile)
    # kernel.shell.user_ns['kernel'] = kernel

    # app.quit()
    # console.kill()
    kernel.io_loop.stop()

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
