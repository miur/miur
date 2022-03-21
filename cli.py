import asyncio
from typing import Any, no_type_check

from just.ext.asyncio import cancel_all, enable_debug_asyncio
from just.iji.main import Context

from .app import Application

app: Application


def main(ctx: Context) -> Any:
    """WiP"""
    args = ctx.args
    cmd = args.pop(0) if args else "navi"

    if cmd == "navi":
        parser = __import__("argparse").ArgumentParser()
        # parser.add_argument("dbs", nargs="+")
        parser.add_argument("-o", "--output")
        parser.add_argument("-c", "--completions", action="store_true")
        parser.add_argument("-f", "--overwrite", action="store_true")
        opts = parser.parse_args(args=args)
        kw = {k: v for k, v in vars(opts).items() if k != "dbs"}
        return navi(**kw)
    raise NotImplementedError


def navi(**_kw: Any) -> Any:
    global app
    with Application() as app:
        app.run(enable_debug_asyncio)
    print("clean")


# HACK:(vimspector): allow running this script in debugger
#   ALT: if not __IPYTHON__:
if __name__ == "__main__":
    navi()
    # TODO: sys.exit()


#%% NEED %gui asyncio
@no_type_check
def _live():
    enable_debug_asyncio(True)
    __import__("atexit").register(lambda: enable_debug_asyncio(False))

    global app
    app = Application()
    app.startup()
    app.canvas.resize(58, 17)
    # l = app.dom._items[154]._data["Optional Deps"]

    def _quit():
        app.shutdown()
        del app

    def _debug():
        cancel_all()
        asyncio.all_tasks()
