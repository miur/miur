import os
import os.path as fs
from functools import cache
from types import ModuleType
from typing import cast

import _curses as C

from ..curses_ext import g_style as S
from ..curses_ext import termcolor2
from ..util.exchook import log_exc
from ..util.logger import log
from .entity_base import Addressable
from .entries import ErrorEntry, FSEntry
from .vlst_base import SatelliteViewport_DataProtocol

# TRY: split into single-dispatch generic functions to draw elements
#   i.e. rev-dep for isolated curses:draw(XXX) inof distributed XXX.draw_curses()


@cache
def ranger_ansi() -> ModuleType | None:
    if __import__("sys").flags.isolated:
        __import__("site").main()  # lazy init for "site" in isolated mode
    try:
        from ranger.gui import ansi
    except Exception as exc:
        log.error("You need to monkey-patch 'curses.setupterm()' in .venv")
        log_exc(exc)
        return None
    return ansi  # type:ignore


def resolve_colorscheme(item: Addressable) -> int:
    attr = S.item
    if isinstance(item, FSEntry):
        path = item.loci
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


class SatelliteViewport_RedrawMixin:
    # pylint:disable=too-many-statements,too-many-branches,too-many-locals
    def redraw(
        self: SatelliteViewport_DataProtocol,
        stdscr: C.window,
        *,
        numcol: bool = True,
    ) -> tuple[int, int]:
        # draw_footer(stdscr)
        # ARCH:WARN: we actually need to render whatever is *shown in viewport* (even if cursor is far outside)
        #   COS: when cursor is outside -- most "write" actions will be disabled
        #   => you always need to know the span of items present in viewport to be rendered in O(1)

        vy, vx = self._viewport_origin_yx
        if not self._lst:
            ## [_] DECI!. insert proper "EMPTY" nodes
            ##   OR pass whole _view into SatVP to access _ent ?
            ##   ALT:BET? prevent whole redraw() inside root_wdg()
            # if fs.isdir(emptylist._originator):
            #   msg = "EMPTY DIR"
            stdscr.addstr(vy, vx, "<<EMPTY>>", S.error | S.cursor)
            return vy, vx

        ## CHECK: if more than one redraw per one keypress
        # log.verbose(f"list: [<={vp.h}/{len(lst)}]")

        # self._viewport_margin_lines
        ci = self._cursor_item_lstindex
        vh = self._viewport_height_lines
        vw = self._viewport_width_columns
        top_idx = self._viewport_followeditem_lstindex

        # SUM:(cy,cx): real cursor pos (either focused item or top/bot linebeg)
        #   DFL: we assume cursor is above/below viewport, unless loop confirms otherwise
        cy = vy if ci < top_idx else vy + vh
        cx = vx

        # WARN! we assume that: { top of NaviWidget = top of RootWidget = 0,0 }
        top_y = self._viewport_followeditem_linesfromtop
        while top_idx > 0 and top_y > 0:
            top_idx -= 1
            top_y -= self._itemheight(self._lst[top_idx])
        # log.trace(f"{top_y} {top_idx=}")

        last = len(self._lst) - 1
        i, y = top_idx, top_y
        while i <= last and y < vh:
            item = self._lst[i]
            if isinstance(item, ErrorEntry):
                stdscr.addstr(vy + y, vx + 0, f"[ {item.name} ]", S.error | S.cursor)
                # HACK:WKRND: hover cursor on error, unless we already looped through cursor
                #   >> meaning "cx" now become indented
                if cx == vx:
                    cy = vy + y
                i += 1
                continue  # RND: draw both err and lst interchangeably

            if numcol:
                rel = i - top_idx
                pfxrel = f"{1+rel:02d}| "
                # TODO: for binary/hex show "file offset in hex" inof "item idx in _xfm_list"
                pfxidx = f"{1+i:03d}{">" if i == ci else ":"} "
                indent = len(pfxrel) + len(pfxidx)
            else:
                indent = 0

            nm, *lines = item.name.split("\n")
            py = y

            # FIXME:RELI: resize(<) may occur during redraw loop, invalidating "vh"
            if 0 <= y < vh:
                stdscr.move(vy + y, vx + 0)
                if numcol:
                    stdscr.addstr(pfxrel, S.pfxrel)
                    stdscr.addstr(pfxidx, S.cursor if i == ci else S.pfxidx)
                xoff = vx + indent  # OR: _, xoff = stdscr.getyx()

                if i == ci:
                    cy = vy + y
                    cx = xoff - 1

                ansi = ranger_ansi()
                if not ansi or len(ansi.split_ansi_from_text(nm)) <= 1:
                    c_schema = resolve_colorscheme(item)
                    stdscr.addnstr(
                        nm,
                        vw - xoff,  # OR: lim = stdscr.getmaxyx()[1] - stdscr.getyx()[1]
                        c_schema | S.cursor if i == ci else c_schema,
                    )
                else:
                    ## ALT:(messy decoding): simply use non-curses libs
                    #  * https://github.com/peterbrittain/asciimatics
                    #  * https://github.com/urwid/urwid
                    #  * ...
                    nm_visible = ansi.char_slice(nm, 0, vw - xoff)
                    # RND: we allow curses to calc() offset by itself
                    stdscr.move(vy + y, xoff)
                    pattr = 0
                    for chunk in ansi.text_with_fg_bg_attr(nm_visible):
                        # log.trace(chunk)
                        if isinstance(chunk, tuple):
                            fg, bg, attr = chunk
                            if i == ci:
                                attr |= S.cursor
                            pattr = termcolor2(fg, bg) | attr
                            # log.info(pattr)
                        else:
                            stdscr.addstr(chunk, pattr)

            # SUM: draw rest of multiline item (2nd line onwards)
            y += 1
            for l in lines:
                if 0 <= y < vh:
                    # stdscr.addstr(vy+y, 2, "|", S.pfxrel)
                    xoff = vx + indent + 2
                    stdscr.addstr(
                        vy + y,
                        xoff,
                        l[: vw - xoff],
                        S.cursor if i == ci else S.iteminfo,
                    )
                y += 1
            if y - py != self._itemheight(item):
                log.error(f"{y - py} != {self._itemheight(item)}")
                raise RuntimeError("WTF: this exception is silently ignored")
            i += 1

        ## ALT:NOTE: draw cursor AGAIN after footer (i.e. over-draw on top of full list)
        ##   NICE: no need to hassle with storing cursor prefix length for cx/cy
        ##   NICE: can redraw only two lines (prev item and cursor) inof whole list
        # cx = len(_pfx(vctx.wndcurpos0))
        # _draw_item_at(vctx.wndcurpos0, citem)
        # stdscr.move(vctx.wndcurpos0, cx)
        ## OR:BET: only change attributes of already printed line
        # cx = len(_pfx(vctx.wndcurpos0))
        # cn = len(lst[vctx.wndcurpos0 + vctx.wndabsoff0].name)
        # stdscr.chgat(vctx.wndcurpos0, cx, cn, ccurs)
        return cy, cx
