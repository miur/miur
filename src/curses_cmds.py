import os
from typing import Callable, Sequence

import _curses as C

from . import curses_ext as CE
from .app import AppGlobals
from .loop_asyncio import asyncio_primary_out
from .util.logger import log

# def raise_(exc: BaseException) -> None:
#     raise exc


def exitloop(g: AppGlobals) -> None:
    # raise SystemExit()  # OR:(same): import sys; sys.exit()
    g.doexit()


def resize(g: AppGlobals) -> None:
    ## HACK:SRC: https://stackoverflow.com/questions/1022957/getting-terminal-width-in-c
    ##   >> make curses to calc sizes by itself (as it does on each .refresh)
    # HACK: force size reinit (as ncurses only does it in .iniscr())
    # SRC:OLD: https://post.bytes.com/forum/topic/python/541442-curses-and-resizing-windows
    C.def_prog_mode()
    C.endwin()
    g.stdscr.refresh()
    # HACK: remove KEY_RESIZE from queue to avoid multi-refresh after several resizes
    #   ALT:FAIL: _ch = g.stdscr.getch(); assert _ch == C.KEY_RESIZE, _ch
    C.flushinp()

    log.info("resize: [{}x{}]".format(*g.stdscr.getmaxyx()))
    ## DEP:configure(--enable-sigwinch)
    # BAD: does not work inside jupyter
    # BAD: should press some key before ev:410 will be reported
    #   ::: it's due to epoll() doesn't listen for SIGWINCH
    #     SEE: /usr/lib/python3.12/site-packages/pexpect/pty_spawn.py
    #   2024-05-11 BUT it works if get_wch() used directly
    # REGR: redraw() during KEY_RESIZE results in ncurses crash
    #   THINK: how to prevent/block redraw in that case?
    g.stdscr.clear()  # CHECK:NEED:OR:NOT? e.g. to clear bkgr (which earlier wasn't redrawn on resize)
    g.root_wdg.resize(g.stdscr)
    g.root_wdg.redraw(g.stdscr)
    g.stdscr.refresh()


def shell_out(g: AppGlobals, loci: str) -> None:
    asyncio_primary_out(g, CE.shell_async(g.stdscr, f=loci))


def shell_out_prompt(g: AppGlobals, loci: str) -> None:
    # HACK※⡢⣯⢁⢏ pre-fill prompt (inof running) by specified cmdline on ZSH startup
    # RQ:(~/.zshrc): if [[ -n ${ZSH_BUFFER-} ]]; then print -z "$ZSH_BUFFER" && unset ZSH_BUFFER; fi
    #   [_] BET: print help notice  above shell-out to use $F,$D,$X,$S vars in cmdline
    #   ALSO: use $f2 to refer to 2nd tab, and $F to refer to previously opened tab
    asyncio_primary_out(g, CE.shell_async(g.stdscr, ZSH_BUFFER=loci, f=loci))


def run_editor(g: AppGlobals, *args: str | Sequence[str]) -> None:
    cmdv = [os.getenv("EDITOR", "nvim")]
    for a in args:
        if isinstance(a, str):
            cmdv.append(a)
        else:
            cmdv.extend(a)
    # TODO: when open dir -- focus netrw on same file, as dir's cached view (if present)
    asyncio_primary_out(g, CE.shell_async(g.stdscr, cmdv))


# TBD: feed non-FSEntry or mixed/virtual Entities into vim (+pre-filter pre-sorted lists)
#   i.e. when current Entity isn't .isdir() -- you need to pass the list through stdin
def run_quickfix(g: AppGlobals, *args: str) -> None:
    # TODO: focus/position cursor in both buffer and in quickfix on same item
    #   * CASE: list of filenames -- buffer on file, no quickfix
    #   * CASE: list of search results -- quickfix on result, hide/delete original buffer
    return run_editor(
        g,
        "-",
        ("-c", "setl noro ma bt=nofile nowrap"),
        ("-c", "au User LazyPluginsLoaded cgetb|copen"),
        args,
    )


# [_] #visual FIXME: don't dump logs into shell on altscreen, when jumping into fullscreen app
#   &why it creates significant delay and glimpses text behind the screen unnecessarily
#   ALT: force somehow TERM to not refresh screen contents when doing altscreen with app
#   ALSO: same when you quit ranger -- it should exit "less cleanly" to not repaint the TERM
def run_ranger(g: AppGlobals, path: str) -> None:
    if int(v := os.getenv("RANGER_LEVEL", "0")) > 0:
        log.warning("skipping cmd due to RANGER_LEVEL=" + v)
        return  # NOTE: exit ranger-nested shell

    import os.path as fs

    # MAYBE: strip loci to be an actual path (even for text file lines)
    if not fs.exists(path):
        raise ValueError(path)

    # TRY: replace XDG file by some e.g. fd=3 redir
    tmp = os.getenv("XDG_RUNTIME_DIR", "/tmp") + "/ranger/cwd"
    # INFO:(--selectfile): it's obsoleted by pos-args, according to "man ranger"
    #   BUT! if you pass a dir in pos-args -- it will be *opened*, inof *selected*
    cmdv = ["ranger", "--choosedir=" + tmp, "--selectfile=" + path]

    def _cb() -> None:
        try:
            # TRY:BET? inof per-app filecache do 'readlink /proc/<child>/cwd' on child process
            #   XLR: can we handle SIGCHLD by background asyncio and read cwd from dying child ?
            with open(tmp, encoding="utf-8") as f:
                cwd = f.read(1024)
            from .ui.entries import FSEntry

            # pylint:disable=protected-access
            if cwd != fs.dirname(path):  # OR: g.root_wdg._navi._view._ent.loci
                g.root_wdg._navi.view_jump_to(FSEntry(cwd))
                resize(g)  # <COS: term could have been resized when using nested app
            # ALT: if os.exists(cwd) and cwd != os.getcwd():
            #     os.chdir(cwd)
        except Exception:
            pass

    asyncio_primary_out(g, CE.shell_async(g.stdscr, cmdv), cb=_cb)


def ipykernel_start(g: AppGlobals) -> None:
    from .util.jupyter import inject_ipykernel_into_asyncio

    loop = __import__("asyncio").get_running_loop()
    # pylint:disable=protected-access
    myns = {"g": g, "stdscr": g.stdscr, "_main": g._main}
    inject_ipykernel_into_asyncio(loop, myns)


def ipyconsole_out(g: AppGlobals) -> None:
    async def _ipy_async() -> None:
        from .util.jupyter import ipyconsole_async

        with CE.curses_altscreen(g.stdscr):
            await ipyconsole_async()

    asyncio_primary_out(g, _ipy_async())


def ipython_out(g: AppGlobals) -> None:
    CE.ipython_out(g.stdscr)


# ALT:BET: allow direct access to contained _objects methods ?
#   i.e. remove "_" private prefix from *._navi._view.*
#   &why to easily control substructures by global/modal keymaps
#   [_] TRY:FIND: better way to achieve that, e.g. type-checked function
#     OR `<Some>Command dispatched by top-class deeper into bus of listening nested objects
#       IDEA: declare `Protocol interfaces for internal classes
#         and combine them as a type for root_wdg dispatch function
#         NICE: we can type-check all dispatched "messages" with their args as actual fn calls
# ALT: match to string, and then resolve to appropriate function
g_input_handlers: dict[str | int, Callable[[AppGlobals], None]] = {
    # pylint:disable=protected-access
    # C.KEY_RESIZE: resize,
    "^[": exitloop,  # <Esc>
    "q": exitloop,
    # "s": shell_out,  # CE.shell_out
    "s": lambda g: shell_out(g, g.root_wdg._navi._view._wdg.focused_item.loci),
    # "a": shell_out_prompt,
    "S": lambda g: shell_out_prompt(g, g.root_wdg._navi._view._wdg.focused_item.loci),
    # "o": run_quickfix,
    "E": run_quickfix,
    "e": lambda g: run_editor(g, g.root_wdg._navi._view._wdg.focused_item.loci),
    "r": lambda g: run_ranger(g, g.root_wdg._navi._view._wdg.focused_item.loci),
    # "R": lambda g: run_ranger(g, g.root_wdg._navi._view._ent.loci),
    "K": ipykernel_start,
    "I": ipyconsole_out,
    "^I": ipython_out,  # <Tab>
    "^J": run_editor,  # <CR>
    "^L": resize,  # <C-l> manually trigger redraw
    "^R": lambda g: g.root_wdg._navi._view.fetch(),  # <C-r> refresh cached list
    "j": lambda g: g.root_wdg._navi.cursor_step_by(1),
    "k": lambda g: g.root_wdg._navi.cursor_step_by(-1),
    "g": lambda g: g.root_wdg._navi.cursor_jump_to(0),
    "G": lambda g: g.root_wdg._navi.cursor_jump_to(-1),
    "h": lambda g: g.root_wdg.view_go_back(),
    "l": lambda g: g.root_wdg.view_go_into(),
}


def handle_input(g: AppGlobals) -> None:
    wch = g.stdscr.get_wch()
    if isinstance(wch, str) and ord(wch) < 20:
        wch = C.unctrl(wch).decode("utf-8")

    # IDEA: partially restore TTY to preserve NLs in unexpected exc/backtrace
    #  C.nocbreak() ... C.cbreak()
    cmd = g_input_handlers.get(wch, None)
    if cmd:
        if (nm := cmd.__name__) == "<lambda>":
            srcbody = __import__("inspect").getsource(cmd).partition("lambda")[2]
            comment = " : " + srcbody.partition(":")[2].strip(" ,\n")
        else:
            comment = f" ({nm})"
    else:
        comment = ""
    log.warning(repr(wch) + comment)
    # print(repr(wch))
    # import sys; sys.stdout.write(repr(wch))
    if cmd:
        # TEMP: don't exit !miur when developing in REPL
        if g.opts.ipykernel:
            try:
                cmd(g)
            except Exception as exc:  # pylint:disable=broad-exception-caught
                from .util.exchook import log_exc

                log_exc(exc)
        else:
            # WARN: last stmt in loop COS: may raise SystemExit
            cmd(g)
        # CHG: only do partial redraw e.g. prev/next cursor areas
        # MAYBE: redraw only if anything had changed (not all cmds to that)
        #   BUT: uix needs visual feedback on each keypress, so it's better to always redraw
        g.root_wdg.redraw(g.stdscr)
        g.stdscr.refresh()
