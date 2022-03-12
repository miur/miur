import asyncio
from typing import Any, no_type_check

from just.ext.asyncio import cancel_all, enable_debug_asyncio

from .app import Application

# BAD: only prints first instance of warning
# from warnings import warn

app: Application


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
    global app
    app = Application()
    app.startup()

    def _quit():
        app.shutdown()
        del app

    def _debug():
        cancel_all()
        asyncio.all_tasks()
