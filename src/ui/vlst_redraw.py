import _curses as C

from ..curses_ext import g_style as S
from ..util.logger import log
from .entries import ErrorEntry
from .itemcolor import text_highlight
from .vlst_base import SatelliteViewport_DataProtocol

# TRY: split into single-dispatch generic functions to draw elements
#   i.e. rev-dep for isolated curses:draw(XXX) inof distributed XXX.draw_curses()


class SatelliteViewport_RedrawMixin:
    # pylint:disable=too-many-statements,too-many-branches,too-many-locals
    def redraw(
        self: SatelliteViewport_DataProtocol,
        stdscr: C.window,
        *,
        numcol: bool | None = None,
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

        if numcol is None:
            # HACK: hide both numcol when viewport is too small
            numcol = vw >= 25

        # SUM:(cy,cx): real cursor pos (either focused item or top/bot linebeg)
        #   DFL: we assume cursor is above/below viewport, unless loop confirms otherwise
        cy = vy if ci < top_idx else vy + vh
        cx = vx

        # WARN! we assume that: { top of NaviWidget = top of RootWidget = 0,0 }
        top_y = self._viewport_followeditem_linesfromtop
        while top_idx > 0 and top_y > 0:
            top_idx -= 1
            top_y -= self._fih(top_idx)
        # log.trace(f"{top_y} {top_idx=}")

        last = len(self._lst) - 1
        i, y = top_idx, top_y
        while i <= last and y < vh:
            item = self._lst[i]
            ent = item._ent

            # MAYBE: make special kind of ErrorWidget and pass rendering to it inof directly here ?
            if isinstance(ent, ErrorEntry):
                stdscr.addstr(vy + y, vx + 0, f"[ {ent.name} ]", S.error | S.cursor)
                # HACK:WKRND: hover cursor on error, unless we already looped through cursor
                #   >> meaning "cx" now become indented
                if cx == vx:
                    cy = vy + y
                i += 1
                continue  # RND: draw both err and lst interchangeably

            if numcol:
                # TODO:OPT: number-column variants:
                #   * [rel]linenum
                #   * [rel]viewpos
                #   * [rel]itemidx
                #   * combined=itemidx+viewpos
                rel = i - top_idx
                pfxrel = f"{1+rel:02d}| "
                # TODO: for binary/hex show "file offset in hex" inof "item idx in _xfm_list"
                pfxidx = f"{1+i:03d}{">" if i == ci else ":"} "
                indent = len(pfxrel) + len(pfxidx)
            else:
                indent = 0

            # WF?: RasterizeViewportItems/CacheDB -> HxW -> StepBy -> RenderVisibleItems
            #   OPT: pre-rasterize everything (more RAM, less CPU/lag during scroll)

            # iw = vw - 2 - indent
            # assert iw > 4
            nm, *lines = item.struct(wrapwidth=vw, maxlines=self._item_maxheight_hint)
            # log.trace(lines)  # <DEBUG:(line split/wrap)
            # nm, *lines = ent.name.split("\n")
            py = y

            # FIXME:RELI: resize(<) may occur during any point in redraw loop, invalidating "vh/vw"
            # wh, ww = stdscr.getmaxyx()
            # assert wh>=vh and ww >= vw
            if 0 <= y < vh:
                stdscr.move(vy + y, vx + 0)
                if numcol:
                    stdscr.addstr(pfxrel, S.pfxrel)
                    stdscr.addstr(pfxidx, S.cursor if i == ci else S.pfxidx)
                xoff = vx + indent  # OR: _, xoff = stdscr.getyx()

                if i == ci:
                    cy = vy + y
                    cx = xoff

                # RND: we allow curses to calc() offset by itself
                stdscr.move(vy + y, xoff)
                lim = vx + vw - stdscr.getyx()[1]
                for chunk, cattr in text_highlight(ent, nm, lim, cursor=i == ci):
                    lim = vx + vw - stdscr.getyx()[1]
                    assert (
                        len(chunk) < lim
                    ), "Err: item.struct() ought to fit text into vw"
                    stdscr.addnstr(chunk, lim, cattr)

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
            if y - py != self._fih(i):
                log.error(f"{y - py} != {self._fih(i)}")
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
