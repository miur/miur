import os
import os.path as fs
from functools import cache
from types import ModuleType
from typing import Iterable, cast

import _curses as C

from ..curses_ext import g_style as S
from ..curses_ext import termcolor2
from ..util.exchook import log_exc
from ..util.logger import log
from .entity_base import Golden
from .entries import FSEntry


@cache
def ranger_ansi() -> ModuleType | None:
    if __import__("sys").flags.isolated:
        __import__("site").main()  # lazy init for "site" in isolated mode
    try:
        from ranger.gui import ansi
    except Exception as exc:  # pylint:disable=broad-exception-caught
        log.error("You need to monkey-patch 'curses.setupterm()' in .venv")
        log_exc(exc)
        return None
    return ansi  # type:ignore


def resolve_colorscheme(ent: Golden) -> int:
    attr = S.item
    if isinstance(ent, FSEntry):
        path = ent.loci
        if not fs.exists(path):
            attr = S.error
            if fs.lexists(path):
                attr |= C.A_ITALIC | C.A_BOLD
        elif fs.isdir(path):
            attr = S.fsdir
            if fs.islink(path):
                # ALT:TRY: use color "in the middle" bw dir and filesymlink
                #   i.e. introduce S.fsdirlink color
                attr = S.fslink | C.A_BOLD
        elif fs.isfile(path):
            attr = S.item
            if fs.islink(path):
                attr = S.fslink | C.A_ITALIC | C.A_BOLD
            elif os.access(path, os.X_OK):
                attr = S.fsexe | C.A_BOLD
    return cast(int, attr)


def colored_ansi_or_schema(
    ent: Golden,
    text: str,
    lim: int,
    *,
    focused: bool = False,
) -> Iterable[tuple[str, int]]:
    ansi = ranger_ansi()
    if not ansi or len(ansi.split_ansi_from_text(text)) <= 1:
        # NICE:IDEA: we can yield multiple colorpairs per filetype too
        #   e.g. for broken symlinks -- highlight existing and not existing parts differently
        c_schema = resolve_colorscheme(ent)

        # BAD:PERF:RND: dim multiline non-ANSI items for more contrasting structure
        # if not ent.name.startswith(text):
        #     cattr = S.iteminfo

        if focused:
            c_schema |= S.cursor
            text += " " * (lim - len(text))  # HACK: make cursor for full viewport width
        yield (text, c_schema)
        return

    ## ALT:(messy decoding): simply use non-curses libs
    #  * https://github.com/peterbrittain/asciimatics
    #  * https://github.com/urwid/urwid
    #  * ...
    nm_visible = ansi.char_slice(text, 0, lim)
    pattr = 0
    for chunk in ansi.text_with_fg_bg_attr(nm_visible):
        # log.trace(chunk)
        if isinstance(chunk, tuple):
            fg, bg, attr = chunk
            if focused:
                attr |= S.cursor
            pattr = termcolor2(fg, bg) | attr
            # log.info(pattr)
        else:
            yield (chunk, pattr)
