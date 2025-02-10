import os
import os.path as fs
from functools import cache
from types import ModuleType
from typing import Any, Iterable, cast
from unicodedata import east_asian_width

import _curses as C

from ..entity.base import Action, Golden
from ..entity.fsentry import FSDir, FSEntry, FSFile, FSLink
from ..util.exchook import log_exc
from ..util.logger import log
from .colorscheme import g_style as S
from .colorscheme import termcolor2


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


def resolve_colorscheme(ent: Golden) -> Any:
    match ent:
        case Action():
            return S.action
        case FSDir():
            if not fs.exists(ent._x.handle):
                return S.error
            if fs.ismount(ent._x.handle):
                return S.fsmnt
            return S.fsdir
        case FSFile():
            path = ent._x.handle
            if not fs.exists(path):
                return S.error
            if os.access(path, os.X_OK):
                return S.fsexe | C.A_BOLD
            return S.item
        case FSLink():
            path = ent._x.handle
            if not fs.exists(path):
                return S.error | C.A_ITALIC | C.A_BOLD
            if fs.isfile(path):
                return S.fslink | C.A_ITALIC | C.A_BOLD
            if fs.isdir(path):
                # ALT:TRY: use color "in the middle" bw dir and filesymlink
                #   i.e. introduce S.fsdirlink color
                return S.fslink | C.A_BOLD
            return S.fslink
        case FSEntry():
            if not fs.exists(ent._x.handle):
                return S.error
            return S.fsdir | C.A_BOLD | C.A_UNDERLINE
        case _:
            # raise TypeError("Unsupported type")
            return S.item


def colored_ansi_or_schema(
    ent: Golden,
    text: str,
    *,
    maxcells: int,
) -> Iterable[tuple[str, int, int]]:
    ansi = ranger_ansi()
    if not ansi or len(ansi.split_ansi_from_text(text)) <= 1:
        # NICE:IDEA: we can yield multiple colorpairs per filetype too
        #   e.g. for broken symlinks -- highlight existing and not existing parts differently
        c_schema = cast(int, resolve_colorscheme(ent))

        # BAD:PERF:RND: dim multiline non-ANSI items for more contrasting structure
        # if not ent.name.startswith(text):
        #     cattr = S.iteminfo

        # FIXED:BAD:PERF: curses.addnstr(,Nbytes,) doesn't understand multi-cell CJK fonts
        if text.isascii():
            text = text[:maxcells]
        else:
            s, nc = "", 0
            for ch in text:
                # BAD: actual char width may depend on !st.render and !font (chosen by !fontconfig)
                #   >> some chars have triple-cell width, and some fonts fit wide chars in single cell
                cw = 2 if (prop := east_asian_width(ch)) == "W" or prop == "F" else 1
                if nc + cw > maxcells:
                    break
                s += ch
                nc += cw
            text = s

        yield (text, c_schema, len(text))  # len=cursor.byteoffset()
        return

    ## ALT:(messy decoding): simply use non-curses libs
    #  * https://github.com/peterbrittain/asciimatics
    #  * https://github.com/urwid/urwid
    #  * ...
    nm_visible = ansi.char_slice(text, 0, maxcells)
    pattr = 0
    boff = len(nm_visible)  # boff = 0
    for chunk in ansi.text_with_fg_bg_attr(nm_visible):
        # log.trace(chunk)
        if isinstance(chunk, tuple):
            fg, bg, attr = chunk
            pattr = termcolor2(fg, bg) | attr
            # log.info(pattr)
        else:
            # TEMP:(boff): always return whole line byteoffset for all chunks
            #   FUT:SEIZE: "ansi.text_with_fg_bg_attr" should yield boff by itself
            yield (chunk, pattr, boff)
