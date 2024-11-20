import os
from typing import Optional, Sequence

import _curses as C

from . import curses_ext as CE
from .app import AppGlobals, KeyTable, g_app
from .loop_asyncio import asyncio_primary_out
from .util.logger import LogLevel, log

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
        except Exception:  # pylint:disable=broad-exception-caught
            pass

    asyncio_primary_out(g, CE.shell_async(g.stdscr, cmdv), cb=_cb)


def ipykernel_start(g: AppGlobals) -> None:
    from .util.jupyter import inject_ipykernel_into_asyncio

    loop = __import__("asyncio").get_running_loop()
    # pylint:disable=protected-access
    myns = {"g": g, "stdscr": g.stdscr, "_main": g._main}
    inject_ipykernel_into_asyncio(loop, myns)


def redirect_logs(g: AppGlobals) -> None:
    from .iomgr import init_explicit_io

    g.opts.logredir = "/t/miur.log"
    init_explicit_io(g)


def ipyconsole_out(g: AppGlobals) -> None:
    async def _ipy_async() -> None:
        from .util.jupyter import ipyconsole_async

        with CE.curses_altscreen(g.stdscr):
            await ipyconsole_async()

    asyncio_primary_out(g, _ipy_async())


def ipython_out(g: AppGlobals) -> None:
    CE.ipython_out(g.stdscr)


def _loci() -> str:
    # pylint:disable=protected-access
    return g_app.root_wdg._navi._view._wdg.focused_item.loci


# ALT:BET: allow direct access to contained _objects methods ?
#   i.e. remove "_" private prefix from *._navi._view.*
#   &why to easily control substructures by global/modal keymaps
#   [_] TRY:FIND: better way to achieve that, e.g. type-checked function
#     OR `<Some>Command dispatched by top-class deeper into bus of listening nested objects
#       IDEA: declare `Protocol interfaces for internal classes
#         and combine them as a type for root_wdg dispatch function
#         NICE: we can type-check all dispatched "messages" with their args as actual fn calls
# ALT: match to string, and then resolve to appropriate function
# TODO: navigate this keymap inside #miur itself
#   NEED: central node with list of all possible initial nodes
_modal_default: KeyTable = {
    # C.KEY_RESIZE: resize,
    "^[": exitloop,  # <Esc>
    "q": exitloop,
    # "s": shell_out,  # CE.shell_out
    "s": lambda g: shell_out(g, _loci()),
    # "a": shell_out_prompt,
    "S": lambda g: shell_out_prompt(g, _loci()),
    # "o": lambda g: run_quickfix(g, _loci()),
    "E": lambda g: run_quickfix(g, _loci()),
    "e": lambda g: run_editor(g, _loci()),
    "r": lambda g: run_ranger(g, _loci()),
    # "R": lambda g: run_ranger(g, g.root_wdg._navi._view._ent.loci),
    "K": ipykernel_start,
    "L": redirect_logs,
    "I": ipyconsole_out,
    "^I": ipython_out,  # <Tab>
    "^J": run_editor,  # <CR>
    "^L": resize,  # <C-l> manually trigger redraw
    # pylint:disable=protected-access
    "^R": lambda g: g.root_wdg._navi._view.fetch(),  # <C-r> refresh cached list
    "j": lambda g: g.root_wdg._navi.cursor_step_by(1),
    "k": lambda g: g.root_wdg._navi.cursor_step_by(-1),
    "g": lambda g: g.root_wdg._navi.cursor_jump_to(0),
    "G": lambda g: g.root_wdg._navi.cursor_jump_to(-1),
    "h": lambda g: g.root_wdg.view_go_back(),
    "l": lambda g: g.root_wdg.view_go_into(),
    C.KEY_HOME: lambda g: g.root_wdg._navi.cursor_jump_to(0),
    C.KEY_END: lambda g: g.root_wdg._navi.cursor_jump_to(-1),
    C.KEY_PPAGE: lambda g: g.root_wdg._navi.cursor_step_by(
        -g.root_wdg._navi._view._wdg._viewport_height_lines // 2
    ),
    C.KEY_NPAGE: lambda g: g.root_wdg._navi.cursor_step_by(
        g.root_wdg._navi._view._wdg._viewport_height_lines // 2
    ),
    # ",": lambda g: modal_switch_to(","),
}

_modal_comma: KeyTable = {
    "s": lambda g: shell_out(g, _loci()),
    "m": lambda g: shell_out(g, _loci()),
}

_modal: dict[str, KeyTable] = {
    **_modal_default,
    ",": _modal_comma,
}


# WKRND: using Optional as {"fwd-type" | None} isn't supported (yet)
#   REF: https://github.com/python/mypy/issues/11582
def modal_switch_to(m: str | Optional[KeyTable], nm: str = "") -> None:
    if m is None:
        nm = nm or "Default"
        t = _modal
    elif isinstance(m, str):
        nm = nm or m
        t = _modal[m]
    elif isinstance(m, dict):
        # nm = nm or m
        t = m
    else:
        raise NotImplementedError()

    g_app.keytablename = nm
    g_app.keytable = t


# TODO: lazy_load
def keytable_insert_aura_pathes() -> None:
    ## Generate key bindings for fast directory jumping
    fpathes = "/d/airy/airy/pathes"
    lst = []
    try:
        with open(fpathes, "r", encoding="utf-8") as f:
            lst = f.readlines()
    except IOError as exc:
        from .util.exchook import log_exc

        log_exc(exc)
        return

    import os.path as fs

    from .ui.entries import FSEntry

    # FIXME: allow ".#"
    entries = [l.partition("#")[0].strip().split(None, 1) for l in lst]
    it = sorted(filter(lambda e: len(e) > 1, entries), key=lambda l: l[0])
    for e in it:
        assert len(e) in [2, 3]
        k, fl, path = e[0], (e[1] if len(e) == 3 else None), e[-1]
        t = _modal
        for s in k[:-1]:
            if s in t:
                if not isinstance(t, dict):
                    raise ValueError("Overlapping keybind vs keytable")
            else:
                t[s] = {}
            t = t[s]
        if not fl:
            pass
        elif fl == "-l":
            lpath = os.readlink(path)
            # NOTE: resolve only basename (relative to its dir)
            if lpath.startswith("/"):
                anchor = fs.realpath(fs.dirname(path))
                relpath = fs.relpath(fs.realpath(path), anchor)
                path = fs.join(fs.dirname(path), relpath)  # MAYBE:USE fs.abspath()
            else:
                path = fs.join(fs.dirname(path), lpath)
        elif fl == "-L":
            path = fs.realpath(path)
        elif fl == "-m":
            from stat import S_ISREG as isfile

            files = __import__("glob").glob(path + "/**", recursive=True)
            path = max(
                (st.st_mtime, x) for x in files if isfile((st := os.stat(x)).st_mode)
            )[1]

        # log.info(_modal["."])
        if k[-1] in t:
            raise ValueError("Conflicting keybind")
        t[k[-1]] = lambda g, v=path: g.root_wdg._navi.view_jump_to(FSEntry(v))


def handle_input(g: AppGlobals) -> None:
    wch = g.stdscr.get_wch()
    if isinstance(wch, str) and ord(wch) < 20:
        wch = C.unctrl(wch).decode("utf-8")

    # IDEA: partially restore TTY to preserve NLs in unexpected exc/backtrace
    #  C.nocbreak() ... C.cbreak()
    loci_override = ""
    cmd = g_app.keytable.get(wch, None)
    if cmd:
        if (nm := getattr(cmd, "__name__", "")) == "<lambda>":
            lns, lnum = __import__("inspect").getsourcelines(cmd)
            mod = __import__("inspect").getmodule(cmd).__name__.partition(".")[2]
            loci_override = f"*{mod}:{lnum}"
            srcbody = ";".join(lns).partition("lambda")[2]
            comment = " : " + srcbody.partition(":")[2].strip(" ,\n")
            # + f"  // :{lnum}"
        else:
            comment = f" ({nm})"
    else:
        comment = ""
    log.at(LogLevel.WARNING, repr(wch) + comment, loci=loci_override)
    # print(repr(wch))
    # import sys; sys.stdout.write(repr(wch))
    if not cmd:
        if g_app.keytable is not _modal:
            modal_switch_to(None)
            g.root_wdg.redraw(g.stdscr)
            g.stdscr.refresh()
    elif isinstance(cmd, dict):
        # modal_switch_to(wch)
        if g_app.keytable == _modal:
            g_app.keytablename = str(wch)
        else:
            g_app.keytablename += " " + str(wch)
        g_app.keytable = cmd
        g.root_wdg.redraw(g.stdscr)
        g.stdscr.refresh()
    elif callable(cmd):
        modal_switch_to(None)
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
    else:
        raise NotImplementedError()
