import os

import _curses as C

from ..curses_ext import g_style as S
from .entity_base import Golden
from .itemcolor import colored_ansi_or_schema
from .rect import Rect


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
    # FIXME: on lastline .replace("\n","⬎") when {maxlines < len(name.splitlines())} to compress short newlines
    # WARN: all contained "\n" should be *preserved* after splitting into individual lines
    def struct(self, wrapwidth: int = 0, maxlines: int = 0) -> list[str]:
        # TEMP:DEBUG: multiline entries
        # nm, *lines = ent.name.split("\n")
        return self._ent.name.replace("o", "o⬎" + os.linesep).splitlines(keepends=True)
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
        # CASE:(rect): crop all text outside of "rect"
        rect: Rect,  # = "visible area of individual item in curses abs coords"
        ## SUM:(off.x,y): itemviewport offset from content/canvas origin
        # offx,  # = for multiline "running block" (i.e. nowrap with \n further than ItemWidth)
        # offs,  # = for one/multiline "running line" (i.e. simply use {nm=ent.name[offs:]})
        offy: int = 0,  # = skip Y lines from multiline body
        ih_hint: int = 1,  # TEMP? pass ViewportPolicy hint, to calculate item.height in same way
        **infoctx: int | None,
    ) -> int:
        stdscr.move(rect.y, rect.x)
        focused = bool(infoctx.get("focused"))

        def lim() -> int:
            nchar = rect.x + rect.w - stdscr.getyx()[1]
            assert nchar > 0
            return nchar

        # TODO:OPT: number-column variants:
        #   * [rel]linenum
        #   * [rel]viewpos
        #   * [rel]itemidx
        #   * combined=itemidx+viewpos

        # HACK: hide both numcol when viewport is too small
        if lim() > 25 and (vpidx := infoctx.get("vpidx")) is not None:
            pfxvp = f"{1+vpidx:02d}| "
            stdscr.addstr(pfxvp, S.pfxrel)

        # TODO: for binary/hex show "file offset in hex" inof "item idx in _xfm_list"
        if lim() > 21 and (lstidx := infoctx.get("lstidx")) is not None:
            # IDEA: shorten long numbers >999 to ‥33 i.e. last digits significant for column
            #   (and only print cursor line with full index)
            pfxlst = f"{1+lstidx:03d}{">" if focused else ":"} "
            stdscr.addstr(pfxlst, S.cursor if focused else S.pfxidx)

        # NOTE: textbody position (to place cursor there)
        _bodyy, bodyx = stdscr.getyx()
        indent = bodyx - rect.x  # ALT =len(pfxvp)+len(pfxlst) | =rect.w-lim()
        boxw = (
            lim() - 1
        )  # = width_of_box_indented_by_prefix_minus_decortail  | ALT=rect.w-indent-1

        # INFO:(rect.h): "self._item_maxheight_hint" is only a recommendation
        #   >> real limitator is either viewport vh
        #     (in which case we crop item, keeping "⬎" inof "…" on last line),
        #   or we adhere to *policy* of individual item on its current maxheight (OR=item.struct.maxlines)
        #     (with "…" char on last line to indicate "end of item")

        # IDEA: don't always wrap unconditionally on "wrapwidth" -- use nowrap for narrow windows {vw<10}
        #   OPT:BET: allow to pan such cropped name left/right manually (OR by "running line")
        lines = self.struct(
            wrapwidth=(boxw if boxw > 10 else 0),
            maxlines=(ih_hint if boxw > 10 else 1),
        )
        # log.trace(lines)  # <DEBUG:(line split/wrap)

        # PERF~BAD? replace by pre-advanced iterator to avoid unnecessary copying
        #   https://stackoverflow.com/questions/11383468/python-iterate-over-a-sublist
        # ALT: insert into loop {if i > rect.h or i < offy: continue; i-=offy}
        if offy:
            assert 0 <= offy < len(lines)
            lines = lines[offy:]
        if len(lines) > rect.h:
            lines = lines[: rect.h]

        # OPT? margins for prefix/postfix on nextlines
        #   - print prefix on a separate first line (heading)
        #     + print postfix too on 1st line or on last line (in trailing)
        #     + print this heading even if skipping several lines by {offy>0}
        #   - print 1st line after prefix and simply indent 2nd line onwards by N
        #     + prepend prefix to whatever next line after skipped by {offy>0}
        #     ! FIXME: item.struct() linewrap should account for linenum prefix
        #   - align 2nd line onwards on prefix for visual boxing
        #     ~ OR: align only on pfxvp but not pfxlst (partially reuse prefix)
        #   - print all lines till the end (minus dedent)
        #   - align all lines onwards on postfix

        # TODO: combine with item.struct() linewrap to split chunks and dup cattr on wrapwidth
        for i, l in enumerate(lines):
            stdscr.move(rect.y + i, bodyx)  #  ALT=(,rect.x+2)

            # MOVE: it only has sense for plaintext items -- nested structures don't have ANSI, do they?
            #   SPLIT:VIZ: `CompositeItem, `PlaintextItem, `AnsitermItem : based on originator `Entity
            # NOTE:(boxw): we crop everything outside "rect" area
            #   >> item.struct() ought to pre-wrap text on {boxw < rect.w}
            for chunk, cattr in colored_ansi_or_schema(
                self._ent, l, boxw, focused=focused
            ):
                # WARN: possibly wraps onto next line, if {len(_visual(chunk))>lim()}
                #   BAD:PERF: you should call addnstr(,lim(),) -- and can't pass "boxw" there
                # RND: curses calc() offset for each next addstr() by itself and hope it won't wrap
                stdscr.addstr(chunk, cattr)

            # [_] WIP:CONT
            # MAYBE:PERF: insert "⬎" by item.struct() once inof repeated checks during rendering
            #   FAIL: substruct should be *copyable*, so decortail can't be part of item.struct()
            # if os.linesep in l: l = l.replace(os.linesep, "⬎")
            # SUM: calculate appropriate leading/trailing decortail symbols
            #   ENH: prepend leading "‥" when "offx>0"
            #   ENH: "…" to 1st line when "offs>0"
            # BAD: to apply different hi for decortail -- we shouldn't include it into line body
            # ALT:(len(l)>boxw):FAIL: doesn't work for term ANSI
            # if lim() > 0:
            #     tail = "⬎" if l.endswith(os.linesep) else "↩"
            # else:
            #     tail = "‥" if i < len(lines) - 1 else "…"
            # stdscr.addstr(tail, S.auxinfo)

        return indent
