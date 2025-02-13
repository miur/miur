from functools import cache
from multiprocessing import Process
from typing import TYPE_CHECKING, Callable, Optional

import _curses as C

from . import curses_ext as CE
from .app import AppGlobals, KeyTable, g_app
from .util.logger import log

if TYPE_CHECKING:
    from types import ModuleType

    from .ui.navi import NaviWidget
    from .ui.view import EntityView


@cache
def M(mod: str) -> "ModuleType":
    import importlib

    return importlib.import_module(mod, __package__)


def _navi() -> "NaviWidget":
    # pylint:disable=protected-access
    return g_app.root_wdg._navi


def _view() -> "EntityView":
    # pylint:disable=protected-access
    return _navi()._view


def _dir() -> str:
    # pylint:disable=protected-access
    return _view()._ent.loci


def _loci() -> str:
    # pylint:disable=protected-access
    return _view()._wdg.focused_item._ent.loci


def _scrollby(ratio: float) -> Callable[[AppGlobals], None]:
    vnl = lambda r: int(_view()._wdg._viewport_height_lines * r)
    return lambda g: _navi().cursor_step_by(vnl(ratio))


# WARN: don't use Thread as GUI thread should be never destroyed! (even after app.quit())
g_render: dict[str, Process] = {}


def spawn_render(nm: str) -> None:
    import atexit

    log.info(nm)
    if nm in g_render:
        if g_render[nm].is_alive():
            log.warning(f"render={nm} is already running! ignored")
            return
        # log.error(f"Err: render={nm} GUI thread should be never destroyed!")
        # return
        atexit.unregister(g_render[nm].join)
        g_render[nm].join()

    def _gui_fork() -> None:
        # TEMP:HACK: exit running miur copy
        # OR: import asyncio; asyncio.get_running_loop().close()
        g_app.doexit()

        # NOTE: import render in new process/thread
        #   COS Qt registers MainThread on import
        mod = M(".ui.render." + nm)
        log.warning(f"forked={mod.__file__}")
        mod.main()

        # [_] FAIL: no exception from here when "sys" wasn't imported
        #   FIND: how to process errors from multiprocessing
        # sys.exit(M(".ui.render." + nm).main())

        ## MAYBE: clean start with same .py interpreter/flags
        # cmd = [.util.devenv.get_py_args()[0], mod.__file__]
        # os.execv(cmd[0], cmd)

    # [_] BUG: when closing QApp, it spawns AGAIN due to asyncio/etc.
    g_render[nm] = Process(target=_gui_fork)
    # g_render[nm].daemon = True
    g_render[nm].start()
    atexit.register(g_render[nm].join)


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
_modal_generic: KeyTable = {
    # C.KEY_RESIZE: resize,
    # "^[": lambda g: g.doexit(),  # <Esc>
    "q": lambda g: g.doexit(),
    "s": lambda g: M(".integ.shell").shell_out(_loci()),
    "a": lambda g: M(".integ.shell").shell_out_prompt(_loci()),
    "S": lambda g: M(".integ.shell").shell_out_prompt(_loci()),
    # "o": lambda g: run_quickfix(g, _loci()),  # CHG: present list of actions
    "E": lambda g: M(".integ.nvim").run_quickfix(),
    "e": lambda g: M(".integ.nvim").run_editor(["--", _loci()]),
    "r": lambda g: M(".integ.ranger").run_ranger(_loci()),
    # "R": lambda g: run_ranger(g, _view()._ent.loci),
    "K": lambda g: M(".integ.jupyter").ipykernel_start(),
    "L": lambda g: M(".integ.jupyter").redirect_logs(),
    "I": lambda g: M(".integ.jupyter").ipyconsole_out(),
    "^I": lambda g: CE.ipython_out(g.stdscr),  # <Tab>
    # "^J": execute_abyss,  # <CR>
    "^L": lambda g: CE.resize(),  # <C-l> manually trigger redraw
    # pylint:disable=protected-access
    "^R": lambda g: _view().fetch(),  # <C-r> refresh cached list
    "j": lambda g: _navi().cursor_step_by(1),
    "k": lambda g: _navi().cursor_step_by(-1),
    "g": lambda g: _navi().cursor_jump_to(0),
    "G": lambda g: _navi().cursor_jump_to(-1),
    "h": lambda g: g.root_wdg.view_go_back(),
    "l": lambda g: g.root_wdg.view_go_into(),
    C.KEY_HOME: lambda g: _navi().cursor_jump_to(0),
    C.KEY_END: lambda g: _navi().cursor_jump_to(-1),
    C.KEY_PPAGE: _scrollby(-0.5),
    C.KEY_NPAGE: _scrollby(+0.5),
    C.KEY_DOWN: lambda g: _navi().cursor_step_by(1),
    C.KEY_UP: lambda g: _navi().cursor_step_by(-1),
    C.KEY_LEFT: lambda g: g.root_wdg.view_go_back(),
    C.KEY_RIGHT: lambda g: g.root_wdg.view_go_into(),
    # ",": lambda g: modal_switch_to(","),
}

_modal_yank: KeyTable = {
    "d": lambda g: M(".integ.xclip").to_system_clipboard(_dir()),
    "p": lambda g: M(".integ.xclip").to_system_clipboard(_loci()),
}

_modal_comma: KeyTable = {
    "s": lambda g: M(".integ.shell").shell_out(_loci()),
    "m": lambda g: M(".integ.shell").shell_out(_loci()),
}

_modal_spawn: KeyTable = {
    "f": lambda g: spawn_render("glfw_imgui"),
    "g": lambda g: spawn_render("qt6gl"),
    "m": lambda g: spawn_render("qt6qml"),
    "n": lambda g: spawn_render("pyqtgr_numpy"),
    "s": lambda g: spawn_render("sdl3gl_imgui"),
    "w": lambda g: spawn_render("qt6wg"),
}


# MOVE: into EmbeddableInputWidget
#   => which can be embedded either into _footer or as 1st item in _vlst.filter_by(?).{input;@history}
# g_inputfield: str = ""  OR g.inputfield
def _start_input(g: AppGlobals) -> None:
    g.inputfield = _view()._filterby
    g.inputpos = len(g.inputfield)  # <RND
    # FIXME: switch to "prev" modal
    # [_] IMPL: allow directly applying some disjoint keymap
    #   BAD:ARCH: we lose ability to introspect/navigate whole keytree
    modal_switch_to(_modal_input, "Input")


def _add_input(g: AppGlobals, wch: str) -> None:
    assert g.inputpos >= 0
    # FAIL:(str is readonly): g.inputfield[g.inputpos : g.inputpos] = wch
    i = g.inputpos
    a = list(g.inputfield)
    a[i:i] = wch
    g.inputfield = "".join(a)
    g.inputpos = i + len(wch)
    ## ALT: contiguous memory
    # a = bytearray(g.inputfield, 'utf-32');
    # a[i:i] = bytearray(wch, 'utf-32')
    # ginp = a.decode('utf-32')
    _view().filter_by(g.inputfield)


def _stop_input(g: AppGlobals) -> None:
    # do_action() -> go up/dn, execute/apply, etc.
    # save_input_to_history()
    g.inputfield = ""
    g.inputpos = -1
    # FIXME: switch to "prev" modal
    modal_switch_to(None)


class NaviMod:
    # pylint:disable=unnecessary-lambda-assignment
    movebkw_linebeg = staticmethod(lambda i, s: (0, s))
    movefwd_lineend = staticmethod(lambda i, s: (len(s), s))
    movebkw_charprev = staticmethod(lambda i, s: (max(0, i - 1), s))
    movefwd_charnext = staticmethod(lambda i, s: (min(len(s), i + 1), s))
    killbkw_linebeg = staticmethod(lambda i, s: (0, s[i:]))
    killfwd_lineend = staticmethod(lambda i, s: (i, s[:i]))
    killbkw_charprev = staticmethod(lambda i, s: ((k := max(0, i - 1)), s[:k] + s[i:]))
    killfwd_charcurs = staticmethod(lambda i, s: (i, s[:i] + s[i + 1 :]))
    # ^T/^Q: movefwd/bkw_wordbeg/end[_space] =
    # ^G/^W: killfwd/bkw_wordbeg/end[_space] =
    # ^Z/^_: undo/redo


def _navimod_input(
    fmod: Callable[[int, str], tuple[int, str]]
) -> Callable[[AppGlobals], None]:
    # [_] BAD: we are losing preview in logs "fmod name in use"
    #   MAYBE: use lambda as before in table?
    def _edit_input(g: AppGlobals) -> None:
        i, s = fmod(g.inputpos, g.inputfield)
        redraw = g.inputfield != s
        g.inputfield = s
        g.inputpos = i
        if redraw:
            _view().filter_by(s)

    return _edit_input


_modal_input: KeyTable = {
    # ALT:(type ERR): provide g.keytablesink = f(g,wch), which is assigned to DFL="cancel_to_rootkeytable()"
    "": _add_input,  # DFL="" -- catch all keypresses
    # REF: /usr/include/curses.h
    # NOTE: emacs-style readline
    "^A": _navimod_input(NaviMod.movebkw_linebeg),
    "^E": _navimod_input(NaviMod.movefwd_lineend),
    "^B": _navimod_input(NaviMod.movebkw_charprev),
    "^F": _navimod_input(NaviMod.movefwd_charnext),
    "^U": _navimod_input(NaviMod.killbkw_linebeg),
    "^K": _navimod_input(NaviMod.killfwd_lineend),
    "^H": _navimod_input(NaviMod.killbkw_charprev),
    "^?": _navimod_input(NaviMod.killbkw_charprev),
    "^D": _navimod_input(NaviMod.killfwd_charcurs),
    "^J": _stop_input,
    "^M": _stop_input,  # WTF: why ^M==^J==CR ?
    # NOTE: windows-style readline
    C.KEY_HOME: _navimod_input(NaviMod.movebkw_linebeg),
    C.KEY_END: _navimod_input(NaviMod.movefwd_lineend),
    C.KEY_LEFT: _navimod_input(NaviMod.movebkw_charprev),
    C.KEY_RIGHT: _navimod_input(NaviMod.movefwd_charnext),
    C.KEY_DL: _navimod_input(NaviMod.killbkw_linebeg),
    C.KEY_EOL: _navimod_input(NaviMod.killfwd_lineend),
    C.KEY_SHOME: _navimod_input(NaviMod.killbkw_linebeg),
    C.KEY_SEND: _navimod_input(NaviMod.killfwd_lineend),
    C.KEY_BACKSPACE: _navimod_input(NaviMod.killbkw_charprev),  # =263:
    C.KEY_DC: _navimod_input(NaviMod.killfwd_charcurs),  # =330: <Delete>
    C.KEY_ENTER: _stop_input,
    # C.KEY_IC: _navimod_input(NaviMod.insertoverwrite_toggle),
    # NOTE: additional actions, which also save/cancel input
    "^G": _stop_input,
    "^C": _stop_input,
    "^[": _stop_input,
    C.KEY_UP: _stop_input,
    C.KEY_DOWN: _stop_input,
    # '\C-x\C-e' edit-command-line
    # SUM: additional controls for current list preview
    "^R": lambda g: _view().sort_by("~"),
    C.KEY_SR: _scrollby(-0.5),
    C.KEY_SF: _scrollby(+0.5),
    C.KEY_PPAGE: _scrollby(-0.5),
    C.KEY_NPAGE: _scrollby(+0.5),
    # "^Y": _yank_inputfield,
    # "^^": _past_inputfield,
    # "^O": _yank_vlstnames,
    # '^I'/TAB: wordexpand-or-complete
    # "^S"/"^T"/"M-f": _navimod_input(NaviMod.findfwd/bkw_char),
}


g_modal_default: KeyTable = _modal_generic | {
    "f": _start_input,
    "y": _modal_yank,
    ",": _modal_comma,
    # BUG: some CTRL-keys are printed as hex i.e. '\x14' inof '^T'
    # "^T": _modal_spawn,
    "^O": _modal_spawn,
}


# WKRND: using Optional as {"fwd-type" | None} isn't supported (yet)
#   REF: https://github.com/python/mypy/issues/11582
def modal_switch_to(m: str | Optional[KeyTable], nm: str = "") -> None:
    if m is None:
        nm = nm or "Default"
        t = g_app.keytableroot
    elif isinstance(m, str):
        nm = nm or m
        tt = g_app.keytable[m]
        if not isinstance(tt, dict):
            raise ValueError(tt)
        t = tt
    elif isinstance(m, dict):
        # nm = nm or m
        t = m
    else:
        raise NotImplementedError()

    g_app.keytablename = nm
    g_app.keytable = t


def handle_input(g: AppGlobals) -> None:
    wch = g.stdscr.get_wch()
    if isinstance(wch, str):  # and ord(wch) < 0x20:
        wch = C.unctrl(wch).decode("utf-8")

    # IDEA: partially restore TTY to preserve NLs in unexpected exc/backtrace
    #  C.nocbreak() ... C.cbreak()
    loci_override = ""
    cmd = g_app.keytable.get(wch, None)
    # NOTE: use ("": ...) as a "catch_all" (inof DFL: dropping/pass-through to root table)
    catch_all = g_app.keytable.get("", None)
    if not cmd and catch_all:
        cmd = catch_all

    if cmd:
        if (nm := getattr(cmd, "__name__", "")) == "<lambda>":
            lns, lnum = __import__("inspect").getsourcelines(cmd)
            mod = __import__("inspect").getmodule(cmd).__name__.partition(".")[2]
            loci_override = f"*{mod}:{lnum}"
            srcbody = ";".join(lns).partition("lambda")[2]
            comment = " : " + srcbody.partition(":")[2].strip(" ,\n")
            # + f"  // :{lnum}"
        else:
            if not nm and isinstance(cmd, dict):
                nm = "modal_switch_to"
            comment = f" ({nm})"
    else:
        comment = ""
    log.at(log.W, repr(wch) + comment, loci=loci_override)
    # print(repr(wch))
    # import sys; sys.stdout.write(repr(wch))
    # RND: keep current modal when "cmd=catch_all"
    if not cmd:
        if g_app.keytable is not g_app.keytableroot:
            modal_switch_to(None)
            g.root_wdg.redraw(g.stdscr)
            g.stdscr.refresh()
    elif isinstance(cmd, dict):
        assert False
        # modal_switch_to(wch)
        if g_app.keytable == g_app.keytableroot:
            g_app.keytablename = str(wch)
        else:
            g_app.keytablename += " " + str(wch)
        g_app.keytable = cmd
        g.root_wdg.redraw(g.stdscr)
        g.stdscr.refresh()
    elif callable(cmd):
        if cmd is catch_all:
            # NOTE: supply current key as 2nd arg for "catch_all"
            #   BAD: different fn_type for this call than in KeyTable
            cmd(g, wch)
        else:
            if not catch_all:
                modal_switch_to(None)
            cmd(g)  # WARN: should be last stmt in loop COS: may raise SystemExit
        ## NOTE: don't redraw on .doexit() for PERF: faster exit
        #   (unless exit is blocked by slow bkgr cleanup -- then redraw only spinner OR manually)
        if not g.exiting:
            # CHG: only do partial redraw e.g. prev/next cursor areas
            # MAYBE: redraw only if anything had changed (not all cmds to that)
            #   BUT: uix needs visual feedback on each keypress, so it's better to always redraw
            g.root_wdg.redraw(g.stdscr)
            g.stdscr.refresh()
    else:
        raise NotImplementedError()


# TRY:MOVE: into src/__init__.py
#   COS:IDEA: automatically run this code when I jupyter reloads files
#   FAIL: "__init__" isn't executed on reload
#   [_] ALT:IDEA: name file "_live.py" and force jupyter to always source it too
def _live() -> None:
    ## HACK: only load code below when working in !jupyter
    # if not hasattr(__import__("__main__"), "__file__"):
    # if hasattr(__import__("builtins"), "__IPYTHON__"):
    # if hasattr(__builtins__, "__IPYTHON__"):
    # if 'JPY_PARENT_PID' in os.environ:
    from .app import g_app as g

    log.debug("<reload>")
    ntf = "reload=%.3f" % log.ts
    vh = g.stdscr.getmaxyx()[1]
    g.stdscr.addnstr(0, vh - len(ntf), ntf, len(ntf))
    g.stdscr.refresh()
