from types import ModuleType
from typing import Optional

import _curses as C

from . import curses_ext as CE
from .app import AppGlobals, KeyTable, g_app
from .util.logger import log


def M(mod: str) -> ModuleType:
    import importlib

    return importlib.import_module(mod, __package__)


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
_modal_generic: KeyTable = {
    # C.KEY_RESIZE: resize,
    "^[": lambda g: g.doexit(),  # <Esc>
    "q": lambda g: g.doexit(),
    "s": lambda g: M(".integ.shell").shell_out(_loci()),
    "a": lambda g: M(".integ.shell").shell_out_prompt(_loci()),
    "S": lambda g: M(".integ.shell").shell_out_prompt(_loci()),
    # "o": lambda g: run_quickfix(g, _loci()),  # CHG: present list of actions
    "E": lambda g: M(".integ.nvim").run_quickfix(),
    "e": lambda g: M(".integ.nvim").run_editor(["--", _loci()]),
    "r": lambda g: M(".integ.ranger").run_ranger(_loci()),
    # "R": lambda g: run_ranger(g, g.root_wdg._navi._view._ent.loci),
    "K": lambda g: M(".integ.jupyter").ipykernel_start(),
    "L": lambda g: M(".integ.jupyter").redirect_logs(),
    "I": lambda g: M(".integ.jupyter").ipyconsole_out(),
    "^I": lambda g: CE.ipython_out(g.stdscr),  # <Tab>
    # "^J": execute_abyss,  # <CR>
    "^L": lambda g: CE.resize(),  # <C-l> manually trigger redraw
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
    C.KEY_DOWN: lambda g: g.root_wdg._navi.cursor_step_by(1),
    C.KEY_UP: lambda g: g.root_wdg._navi.cursor_step_by(-1),
    C.KEY_LEFT: lambda g: g.root_wdg.view_go_back(),
    C.KEY_RIGHT: lambda g: g.root_wdg.view_go_into(),
    # ",": lambda g: modal_switch_to(","),
}

_modal_comma: KeyTable = {
    "s": lambda g: M(".integ.shell").shell_out(_loci()),
    "m": lambda g: M(".integ.shell").shell_out(_loci()),
}

g_modal_default: KeyTable = _modal_generic | {
    ",": _modal_comma,
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
            if not nm and isinstance(cmd, dict):
                nm = "modal_switch_to"
            comment = f" ({nm})"
    else:
        comment = ""
    log.at(log.W, repr(wch) + comment, loci=loci_override)
    # print(repr(wch))
    # import sys; sys.stdout.write(repr(wch))
    if not cmd:
        if g_app.keytable is not g_app.keytableroot:
            modal_switch_to(None)
            g.root_wdg.redraw(g.stdscr)
            g.stdscr.refresh()
    elif isinstance(cmd, dict):
        # modal_switch_to(wch)
        if g_app.keytable == g_app.keytableroot:
            g_app.keytablename = str(wch)
        else:
            g_app.keytablename += " " + str(wch)
        g_app.keytable = cmd
        g.root_wdg.redraw(g.stdscr)
        g.stdscr.refresh()
    elif callable(cmd):
        modal_switch_to(None)
        # MOVE: extract try-catch into wrapper assigned to "g.ui.handle_input"
        # TEMP: don't exit !miur when developing in REPL
        # FUT: always do this, even for regular usage (i.e. stability for end-user)
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
