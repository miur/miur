import asyncio
import curses as C
from typing import Any, no_type_check

from just.ext.asyncio import cancel_all, enable_debug_asyncio

from .app import Application
from .curses.output import CursesOutput
from .dom import CursorViewWidget
from .fragments import handle_keybindings

# BAD: only prints first instance of warning
# from warnings import warn

# HACK:(vimspector): allow running this script in debugger
if __name__ == "__main__":
    navi()


def navi(**_kw: Any) -> Any:
    with Application(run) as app:
        app.run(enable_debug_asyncio)
    print("clean")


# [_] SEIZE: A Python asyncio cancellation pattern | by Rob Blackbourn | Medium ⌇⡢⠨⢣⣉
#   https://rob-blackbourn.medium.com/a-python-asyncio-cancellation-pattern-a808db861b84
async def run(app: Application) -> None:
    # OLD: fstop = cast(asyncio.Task, asyncio.current_task()).cancel
    # ALT:NICE: works even in Jupyter
    fstop = app.cancel

    scr = app.tui.scr
    draw = CursesOutput(scr, app.wg)
    scr.nodelay(True)  # non-blocking .getch()
    loop = asyncio.get_running_loop()
    STDIN_FILENO = 0
    cb = lambda: process_input(scr, app.wg, fstop, draw)
    loop.add_reader(fd=STDIN_FILENO, callback=cb)

    try:
        await draw.drawloop()
    finally:
        loop.remove_reader(fd=STDIN_FILENO)


def process_input(
    scr: C.window, wg: CursorViewWidget, fstop: Any, tui: CursesOutput
) -> None:
    # NOTE: only count events, don't accumulate into list for post-processing
    #   << otherwise new events received during processing will become stalled
    #   FUTURE:TRY: read events in bursts (nested while loop) to be able to cancel-out
    #     BAD:PERF: we may constantly linger in this loop, never yielding to asyncio
    #     MAYBE: current single loop already allows "bursts" and giving up CPU time ?
    evnum = 0
    # HACK:(getch-ungetch-getkey): avoid throwing exception
    while (ch := scr.getch()) != C.ERR:
        evnum += 1
        # if ch == C.KEY_RESIZE:
        #     print(ch)
        C.ungetch(ch)
        key = scr.get_wch()
        # key = scr.getkey()
        print(repr(key), file=__import__("sys").stderr)

        ## DEP:configure(--enable-sigwinch)
        # BAD: does not work inside jupyter
        # BAD: should press some key before ev:410 will be reported
        # REGR: redraw() during KEY_RESIZE results in ncurses crash
        #   THINK: how to prevent/block redraw in that case?
        if key == C.KEY_RESIZE:
            tui.resize()
            continue

        if key in ("q", "\033"):  # "d",
            fstop()
        handle_keybindings(wg, key)
        tui.invalidate()

    if evnum == 0:
        print("WTF: processing woke up w/o input", file=__import__("sys").stderr)


#%% NEED %gui asyncio
app: Application


@no_type_check
def _live():
    global app
    app = Application(run)

    app.__enter__().attach()

    # HACK: autoclose newterm() to restore WM visual space
    #   OR: never close newterm() -- to keep WM layout stable
    app.tasks[0].add_done_callback(app.__exit__)
    # TODO app.resize()

    def _quit():
        app.shutdown()
        del app

    def _debug():
        cancel_all()
        asyncio.all_tasks()
