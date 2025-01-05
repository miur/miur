import os
from typing import Sequence, Unpack

from ..util.logger import log
from .any_exe import ExeKWArgs, run_tty_async, to_cmdv


def run_editor(cmdv: Sequence[str], /, **kw: Unpack[ExeKWArgs]) -> None:
    return run_tty_async([os.getenv("EDITOR", "nvim"), *cmdv], **kw)


def _render_lst() -> tuple[str, int, str | None]:
    from ..app import g_app

    ## BET?NICE:IDEA: construct and feed vimscript or .lua to setqflist()
    #   ++ more freedom on all options specified
    #   BET: supports multiline entries
    ## ALT: pass everythin in arg {_lst:list[str],i:int}
    # pylint:disable=protected-access
    wdg = g_app.root_wdg._navi._view._wdg
    # IDEA: populate qf list with <lnum:col> from cached `View for the nodes
    lst = os.linesep.join(x.name for x in wdg._lst)
    idx = 1 + wdg._cursor_item_lstindex
    name = wdg.focused_item._ent.name if wdg._lst else None
    return lst, idx, name


# CHECK:TODO: focus/position cursor in both buffer and in quickfix on same item
#   * CASE: list of filenames -- buffer jumped to file, no quickfix
#     ~~ so lines could be re-interpreted by explicit actions from user
#   * CASE: list of search results -- quickfix on result, hide/delete original buffer
#     == meaning we pick initial interpretation inof the user
# FUT:SPLIT: feed_lst_into_stdin(formatter) && read_quickfix_from_stdin(nvim_cmdv)
# TBD: feed non-FSEntry or mixed/virtual Entities into vim (+pre-filter pre-sorted lists)
#   i.e. when current Entity isn't .isdir() -- you need to pass the list through stdin
def run_quickfix() -> None:
    # TODO: when open dir -- focus netrw on same file, as dir's cached view (if present)
    #   "+/^nm"
    slst, idx, name = _render_lst()
    log.trace(f"{idx=} : {name=}")
    cmdv = to_cmdv(
        # MAYBE:(cgetb): still load entries in buffer
        #   "-", ("-c", "setl noro ma bt=nofile nowrap"),
        #   ALT:BAD:("cgetfile -"): can't open "-"
        ("--cmd", "set errorformat=%f", "-q-", "-c", "set errorformat&"),
        ## DISABLED:BAD: I don't need to "open" file under cursor, only to focus on it
        # "cc%d" % idx
        ("-c", "au User LazyPluginsLoaded copen" + (" | /\\V" + name if name else "")),
    )
    return run_editor(cmdv, input=slst)
