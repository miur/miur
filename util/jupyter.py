

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
