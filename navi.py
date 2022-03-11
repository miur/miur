import asyncio
from typing import Any, no_type_check

from just.ext.asyncio import cancel_all, enable_debug_asyncio

from .app import Application
from .curses.input import CursesInput
from .curses.output import CursesOutput

# BAD: only prints first instance of warning
# from warnings import warn

# HACK:(vimspector): allow running this script in debugger
if __name__ == "__main__":
    navi()


def navi(**_kw: Any) -> Any:
    with Application(run) as app:
        app.run(enable_debug_asyncio)
    print("clean")


async def run(app: Application) -> None:
    canvas = CursesOutput(app)
    with CursesInput(app, canvas):
        await canvas.drawloop()


#%% NEED %gui asyncio
app: Application


@no_type_check
def _live():
    global app
    app = Application(run)

    app.__enter__().attach()

    # HACK: autoclose newterm() to restore WM visual space
    #   OR: never close newterm() -- to keep WM layout stable
    app.tasks[0].add_done_callback(lambda *_: app.__exit__())
    # TODO app.resize()

    def _quit():
        app.shutdown()
        del app

    def _debug():
        cancel_all()
        asyncio.all_tasks()
