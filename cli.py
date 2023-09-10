## FAIL: always start "ij" from "/t" to avoid including PWD folders as modules
# import sys; sys.path = ['/usr/lib/python3.11', '/usr/lib/python3.11/site-packages' ]

import asyncio
from argparse import ArgumentParser
from typing import Any, no_type_check

from just.ext.asyncio import cancel_all, enable_debug_asyncio
from just.use.iji.main import Context

from .app import Application

app: Application


def cli_spec(parser: ArgumentParser) -> ArgumentParser:
    o = parser.add_argument
    # o("dbs", nargs="+")
    o("-o", "--output")
    o("-c", "--completions", action="store_true")
    o("-f", "--overwrite", action="store_true")
    return parser


def main(ctx: Context) -> Any:
    """WiP"""
    args = ctx.args
    cmd = args.pop(0) if args else "auto"

    opts = cli_spec(ArgumentParser()).parse_args(args=args)
    kw = {k: v for k, v in vars(opts).items() if k != "dbs"}

    assert cmd in ("fm", "text", "auto", "pkgs")
    return navi(cmd, **kw)


def navi(variant: str, **_kw: Any) -> None:
    global app
    with Application(variant) as app:
        app.run(enable_debug_asyncio)
    print("clean")


# HACK:(vimspector): allow running this script in debugger
#   ALT: if not __IPYTHON__:
if __name__ == "__main__":
    raise NotImplementedError
    navi("fm")
    # TODO: sys.exit()


# %% NEED %gui asyncio
@no_type_check
def _live():
    def _init():
        enable_debug_asyncio(True)
        __import__("atexit").register(lambda: enable_debug_asyncio(False))
        global app
        app = Application()
        app.startup()
        # app.__enter__()
        ## app.iodev = stack.enter_context(CursesDevice())  # OR: self.ctx()
        ## app.canvas = CursesOutput(self.iodev, self.wg)
        ## app.hotkey = CursesInput(self, self.iodev, self.canvas)
        # app.attach()
        # app.canvas.resize()
        # app.canvas.resize(58, 17)
        # app.canvas.resize(80, 13)
        # l = app.dom._items[154]._data["Optional Deps"]

    def _quit():
        global app
        app.shutdown()
        del app

    def _debug():
        asyncio.all_tasks()
        cancel_all()
