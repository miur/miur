import os.path as fs

import _curses as C

from ..curses_ext import ColorMap
from ..util.logger import log
from .entries import ErrorEntry, FSEntry
from .vlst_base import SatelliteViewport_DataProtocol


class SatelliteViewport_RedrawMixin:
    # pylint:disable=too-many-statements,too-many-branches,too-many-locals
    def redraw(self: SatelliteViewport_DataProtocol, stdscr: C.window) -> None:
        # draw_footer(stdscr)
        # ARCH:WARN: we actually need to render whatever is *shown in viewport* (even if cursor is far outside)
        #   COS: when cursor is outside -- most "write" actions will be disabled
        #   => you always need to know the span of items present in viewport to be rendered in O(1)
        c_item = C.color_pair(ColorMap.default)
        c_auxinfo = C.color_pair(ColorMap.auxinfo)
        c_iteminfo = C.color_pair(ColorMap.iteminfo)
        c_pfxrel = c_auxinfo
        c_pfxidx = c_iteminfo
        c_cursor = C.A_REVERSE | C.A_BOLD  # OR: C.color_pair(ColorMap.cursor)
        c_error = C.color_pair(ColorMap.error)
        c_fsdir = C.color_pair(ColorMap.fsdir)
        c_fslink = C.color_pair(ColorMap.fslink)

        vy, vx = self._viewport_origin_yx
        if not self._lst:
            ## [_] DECI!. insert proper "EMPTY" nodes
            ##   OR pass whole _view into SatVP to access _ent ?
            ##   ALT:BET? prevent whole redraw() inside root_wdg()
            # if fs.isdir(emptylist._originator):
            #   msg = "EMPTY DIR"
            stdscr.addstr(vy, vx, "<<EMPTY>>", c_error | c_cursor)
            return

        # log.verbose(f"list: [<={vp.h}/{len(lst)}]")

        # self._viewport_margin_lines
        ci = self._cursor_item_lstindex
        vh = self._viewport_height_lines
        vw = self._viewport_width_columns
        top_idx = self._viewport_followeditem_lstindex
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
                stdscr.addstr(vy + y, vx + 0, f"[ {item.name} ]", c_error | c_cursor)
                i += 1
                return

            rel = i - top_idx
            pfxrel = f"{1+rel:02d}| "
            # TODO: for binary/hex show "file offset in hex" inof "item idx in _xfm_list"
            pfxidx = f"{1+i:03d}{">" if i == ci else ":"} "
            indent = len(pfxrel) + len(pfxidx)
            nm, *lines = item.name.split("\n")
            py = y
            if 0 <= y < vh:
                stdscr.addstr(vy + y, vx + 0, pfxrel, c_pfxrel)
                stdscr.addstr(
                    vy + y, vx + len(pfxrel), pfxidx, c_cursor if i == ci else c_pfxidx
                )
                xoff = vx + indent
                if not isinstance(item, FSEntry):
                    c_schema = c_item
                elif fs.islink(item.loci):
                    c_schema = c_fslink
                    if fs.isdir(item.loci):
                        c_schema |= C.A_BOLD
                elif fs.isdir(item.loci):
                    c_schema = c_fsdir
                else:
                    c_schema = c_item
                stdscr.addstr(
                    vy + y,
                    xoff,
                    nm[: vw - xoff],
                    c_schema | c_cursor if i == ci else c_schema,
                )
            y += 1
            for l in lines:
                if 0 <= y < vh:
                    # stdscr.addstr(vy+y, 2, "|", c_pfxrel)
                    xoff = vx + indent + 2
                    stdscr.addstr(
                        vy + y,
                        xoff,
                        l[: vw - xoff],
                        c_cursor if i == ci else c_iteminfo,
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
