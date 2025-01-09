import _curses as C

from ..curses_ext import g_style as S
from ..util.logger import log
from .entries import ErrorEntry
from .rect import Rect
from .vlst_base import SatelliteViewport_DataProtocol

# TRY: split into single-dispatch generic functions to draw elements (./frontend/curses.py)
#   i.e. rev-dep for isolated curses:draw(type:XXX) inof distributed XXX.draw_curses()


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
                y += 1
                continue  # RND: draw both err and lst interchangeably

            # WF?: RasterizeViewportItems/CacheDB -> HxW -> StepBy -> RenderVisibleItems
            #   OPT: pre-rasterize everything (more RAM, less CPU/lag during scroll)

            # FIXME:RELI: resize(<) may occur during any point in redraw loop, invalidating "vh/vw"
            # wh, ww = stdscr.getmaxyx()
            # assert wh>=vh and ww >= vw
            #   ALT:BET:TRY: wrap all stdscr.* calls and raise "EAGAIN" if resize had occured
            #     &why to avoid drawing outside curses if window had shrinked
            #     ALT:BET! delay resize() processing until current draw-frame finishes
            indent = item.render_curses(
                stdscr,
                rect=Rect(w=vw, h=(vh if y < 0 else vh - y), x=vx, y=vy + y),
                offy=(-y if y < 0 else 0),
                ih_hint=self._item_maxheight_hint,
                # infoctx::
                lstidx=i if numcol else None,
                vpidx=(i - top_idx) if numcol else None,
                vpline=y if numcol else None,
                focused=((y - top_y) if i == ci else None),  # CHG> substruct_ptr
            )
            if i == ci:
                cy = vy + y
                cx = vx + indent
            # BAD: desynchronized from default item.height due to ext-supplied "maxlines"
            #   MAYBE: totally eliminate item.height usage -- jump step_by() bw internal structures
            y += self._fih(i)
            i += 1

        ## ALT:NOTE: draw cursor AGAIN after footer (i.e. over-draw on top of full list)
        ##   NICE: no need to hassle with storing cursor prefix length for cx/cy
        ##   NICE: can redraw only two lines (prev item and cursor) inof whole list
        ##   BAD: impossible to override already rendered item (e.g. textlog or opengl)
        # cx = len(_pfx(vctx.wndcurpos0))
        # _draw_item_at(vctx.wndcurpos0, citem)
        # stdscr.move(vctx.wndcurpos0, cx)
        ## OR:BET: only change attributes of already printed line
        # cx = len(_pfx(vctx.wndcurpos0))
        # cn = len(lst[vctx.wndcurpos0 + vctx.wndabsoff0].name)
        # stdscr.chgat(vctx.wndcurpos0, cx, cn, ccurs)
        return cy, cx
