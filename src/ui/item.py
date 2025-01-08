import _curses as C

from ..curses_ext import g_style as S
from .entity_base import Golden
from .itemcolor import text_highlight


## FUT:TRY: make any `*Widget into valid Entity to be destructured and explored
##   class ItemWidget(Golden):
# RENAME?(TextItemWidget): to override .struct returning wrapped text inof composite substructure
class ItemWidget:
    # RENAME? .boxheight
    # def nr_lines(self, wrap: int = 0) -> int:
    #     return self.name.count("\n") + 1

    # OPT:WF: additional increment can be adjusted individually by (+/-/=) pressed on item
    #   e.g. to get more/less preview for a specific folder only, or =3 to get only 3 lines fixed
    #   OR:ALSO: can be adjusted at once for all [visible] items in folder/filetype/mountpoint/global/etc.
    maxheight: int = 0

    # FAIL: cyclic imports for "pull inof push" strategy with .invalidate() + @cached_property + parent link
    #   COS: type of {SatelliteViewport_DataProtocol._lst: ItemWidget}
    # def __init__(self, ent: Golden, *, parent: SatelliteViewport_DataProtocol) -> None:
    def __init__(self, ent: Golden) -> None:
        self._ent = ent

    ## WARN: `ItemWidget is inherent to each individual `SatelliteViewport and can't be shared bw them
    ##   i.e. you may have *same* `Entity shown in two different _lst viewers,
    ##   but corresponding `ItemWidgets may have different HxW .bbox for text,
    ##   or even be two different `ItemWidgets altogether (FSEntryWidget vs FSTreeWidget)
    ##   >> t4 you may safely cache HxW inside `ItemWidget itself, and then resize() in batch
    ##     ~~ meaning, you don't need {@lru_cache: def struct} and then clear_cache() when you change .name

    ## ALSO: allow ItemWidget.height to be more than _itemheight()
    ##   >> i.e. to have empty line after item's text

    ## MAYBE:PERF?
    # @cached_property
    # def struct(...)
    # def name(self):
    #   if ("struct" in obj.__dict__):
    #       delattr(obj, "struct")

    # RENAME? .lines, .boxed, __iter__, __in__
    # [_] ARCH: override for `Dashboard and `FSTree widgets ?
    #   ALT:BET? directly produce grouped oneline TextItems inof single wrapped multiline .name
    #     OR:ENH: make nested `ItemWidget inside `ItemWidget
    #   NICE:USAGE: substruct can be used to select/copy individual elements in table row, e.g. FSEntry filesize
    # MOVE? make ItemWidget to calc() item height and draw it (or only ItemXfm) inof directly drawing Item/Entity
    #   WARN: don't store "index" inside ItemWidget << PERF:(slow): rebuild on each order-by
    #     ~~ though, I can pass list index ctx into ItemWidget.render(ctx[index=i])
    ## NOTE: :maxlines is only a "hint", i.e. if ItemWidget absolutely must -- it can be more than viewport
    # TODO: when setting HxW you should specify if H,W are "fixed/box" or "relaxed/shrink"
    def struct(self, wrapwidth: int = 0, maxlines: int = 0) -> list[str]:
        ## ALSO:IDEA: when wrapwidth=0, insert "‥" at the end of each linepart, which longer than viewport
        ##   OR: smart-compress in the middle each part of .name bw newlines
        ## [_] TODO: !hi last char in each line differently
        # return [
        #     l[c * iw : c * (iw + 1)] + ("↩" if c < maxwrap else "…")
        #     for l in item.name.split("\n")
        #     for c in range((len(l) // iw) + 1)
        #     if c <= maxwrap
        # ]
        lines: list[str] = []
        s = self._ent.name
        c = 0
        # VIZ: separately(maxwrap+maxnewlines), combined(maxwrap<maxlines), unlimited
        while c < len(s) and len(lines) <= maxlines:
            nc = min(c + wrapwidth, len(s) - c)
            if (nn := s.find("\n", c, c + wrapwidth)) >= 0:
                nc = min(nn, nc)
            lines.append(s[c:nc])
            c = nc
        return lines

    # DECI: pass XY to redraw/render ? OR store as .origin ?
    #   ~store~ makes it possible to .redraw() individual elements only
    #     BAD: all XY should be updated each time we scroll :(
    def render_curses(
        self,
        stdscr: C.window,
        absy: int,
        absx: int,
        maxw: int,
        **kw: int | None,
    ) -> int:
        # RND: we allow curses to calc() offset by itself
        stdscr.move(absy, absx)
        cursor = bool(kw.get("cursor"))

        # [_] FIXME: item.struct() linewrap should account for linenum prefix
        # TODO:OPT: number-column variants:
        #   * [rel]linenum
        #   * [rel]viewpos
        #   * [rel]itemidx
        #   * combined=itemidx+viewpos
        if (vpidx := kw.get("vpidx")) is not None:
            pfxvp = f"{1+vpidx:02d}| "
            stdscr.addstr(pfxvp, S.pfxrel)
        # TODO: for binary/hex show "file offset in hex" inof "item idx in _xfm_list"
        if (lstidx := kw.get("lstidx")) is not None:
            # IDEA: shorten long numbers >999 to ‥33 i.e. last digits significant for column
            #   (and only print cursor line with full index)
            pfxlst = f"{1+lstidx:03d}{">" if cursor else ":"} "
            stdscr.addstr(pfxlst, S.cursor if cursor else S.pfxidx)

        # NOTE: textbody position (to place cursor there)
        #  ALT= len(pfxvp) + len(pfxlst)
        _by, bx = stdscr.getyx()

        lim = absx + maxw - bx
        # TODO: combine with item.struct() linewrap to split chunks and dup cattr on wrapwidth
        # MOVE: it only has sense for plaintext items -- nested structures don't have ANSI, do they?
        for chunk, cattr in text_highlight(
            self._ent, self._ent.name, lim, cursor=cursor
        ):
            lim = absx + maxw - stdscr.getyx()[1]
            assert len(chunk) <= lim, "Err: item.struct() ought to fit text into vw"
            stdscr.addnstr(chunk, lim, cattr)

        return bx - absx
