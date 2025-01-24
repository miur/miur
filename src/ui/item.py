import os
from typing import TYPE_CHECKING, Any, Callable, Final

import _curses as C

from ..curses_ext import g_style as S

# from ..util.logger import log
from .entity_base import Golden
from .entries import ErrorEntry, TextEntry
from .itemcolor import colored_ansi_or_schema
from .rect import Rect

if TYPE_CHECKING:
    from .navihistory import EntityViewCachePool


# RENAME?MOVE? EntityView.classify_preview()
# ALT:PERF: do it only once on vlst.assign() and store inside `ItemWidget
def _pick_spacermark(pool: "EntityViewCachePool", ent: Golden) -> tuple[int, str]:
    ## NOTE: preview-hint to be aware of next steps w/o moving cursor
    ## BAD: only shows marker for already cached views, so you NEED to move cursor
    ##   FIXME? gen-preview for all items in list -- BUT:PERF
    # BET:PERF: cache weak fwd-ref to corresponding `EntityView inside `ItemWidget
    # MAYBE:ARCH: access directly to global "g_app.entvpool"
    # pylint:disable=protected-access
    v = pool.get(ent)
    if not v:
        ## DEBUG: when _pool contains different `Entity instance than .explore
        # if ent.name == "user":
        #     log.debug(f"{id(list(self._pool)[3]._ent):x}, {id(ent):x}")  # <DEBUG
        #     log.debug(f"{list(self._pool)[3]._ent == ent}")  # <DEBUG
        return (S.default, "")  # OR=﹖?⹔⸮՞¿
    if not v._orig_lst:  # = if dir/file is truly empty
        return (S.empty, "∅")  # OR=○◌
    if any(isinstance(x._ent, ErrorEntry) for x in v._wdg._lst):
        return (S.error, "‼")  # OR=⁈ ❕❗
    ## BET: modify color of visited mark, COS: it overlaps with all other marks
    if v._visited:  # OR: if ent in self._pool_visited:
        return (S.fsexe, "⋄")  # OR=+↔
    if not v._wdg._lst:  # = if filtered result is empty
        return (S.mark, "⊙")  # OR=⊗⦼⦰ ⦱ ⦲ ⦳
    # = regular non-empty node
    return (S.mark, "*")  # OR=⊕⊛


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
    # FIXME: compress short NLs on lastline (preserving tail NL) when {maxlines < len(name.splitlines())}
    #   :: if os.linesep in l: l = l[:-1].replace([os.linesep, "\n", "\r"], "⬎") + l[-1]
    # WARN: all contained "\n" should be *preserved* after splitting into individual lines
    def struct(
        self,
        wrapwidth: int = 0,
        maxlines: int = 0,
        wrapreserve: int = 0,  # = for decortail | RENAME? extendlast
    ) -> list[str]:
        # TEMP:DEBUG: multiline entries
        NL, NUL = os.linesep, "\0"
        # ALT:(splitlines):FAIL: discards empty lines
        lines = self._ent.name.replace("o", "o" + NL).replace(NL, NL + NUL).split(NUL)
        if not wrapwidth:
            return lines
        maxw = wrapwidth - wrapreserve
        assert maxw > 0
        wrapped: list[str] = []
        for l in lines:
            if len(l) <= maxw:
                wrapped.append(l)
            else:
                for i in range(0, len(l), maxw):
                    if len(l) - i <= wrapreserve:
                        wrapped[-1] += l[i : i + maxw]
                    else:
                        wrapped.append(l[i : i + maxw])
        return lines

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

    # TODO:OPT: number-column variants:
    #   * [rel]linenum
    #   * [rel]viewpos
    #   * [rel]itemidx
    #   * combined=itemidx+[rel]viewpos
    def _render_itemnum_prefix(
        self,
        stdscr: C.window,
        lim: Callable[[], int],
        **infoctx: int | None,
    ) -> int:
        focused = infoctx.get("focusid") is not None
        combined = True  # OPT:TEMP: hardcode
        if combined:
            cidx = infoctx.get("lstidx")
            bidx = infoctx.get("vpidx")
            if isinstance(self._ent, TextEntry):
                if "`0x" in self._ent.loci:
                    bpfx = cpfx = "" if cidx is None else f"{cidx*16:02x}"
                else:
                    bpfx = cpfx = "" if cidx is None else f"{1+cidx:02d}"
            else:  # e.g. FSEntry, etc.
                cpfx = "" if cidx is None else f"{1+cidx:02d}"
                bpfx = "" if bidx is None else f"{1+bidx:02d}"

            if focused:
                if cpfx:
                    stdscr.addstr(f"{cpfx:>3s}> ", S.pfxrel | S.cursor)
            else:
                if bpfx:
                    stdscr.addstr(f"{bpfx:>3s}: ", S.pfxidx)
        else:
            # HACK: hide both numcol when viewport is too small
            if lim() > 25 and (vpidx := infoctx.get("vpidx")) is not None:
                pfxvp = f"{1+vpidx:02d}| "
                stdscr.addstr(pfxvp, S.pfxrel)
            # TODO: for binary/hex show "file offset in hex" inof "item idx in _xfm_list"
            #   ALSO: start offsets from "0x0" inof "1"
            if lim() > 21 and (lstidx := infoctx.get("lstidx")) is not None:
                # IDEA: shorten long numbers >999 to ‥33 i.e. last digits significant for column
                #   (and only print cursor line with full index)
                pfxlst = f"{1+lstidx:03d}{">" if focused else ":"} "
                stdscr.addstr(pfxlst, S.pfxidx | (S.cursor if focused else 0))
        return stdscr.getyx()[1]  # ALT =len(pfxvp)+len(pfxlst) | =rect.w-lim()

    def _render_ansi(self, stdscr: C.window, **kw: Any) -> tuple[int, bool]:
        # MOVE: it only has sense for plaintext items -- nested structures don't have ANSI, do they?
        #   SPLIT:VIZ: `CompositeItem, `PlaintextItem, `AnsitermItem : based on originator `Entity
        # NOTE:(boxw): we crop everything outside "rect" area
        #   >> item.struct() ought to pre-wrap text on {boxw < rect.w}
        cattr = S.cursor
        boff = len(kw["text"])
        for chunk, cattr, boff in colored_ansi_or_schema(**kw):
            # RND: curses calc() offset for each next addstr() by itself
            # WARN: possibly wraps onto next line, if {len(_visual(chunk))>lim()}
            # FAIL:(addnstr(,lim(),)): wrong byte-cropping for multi-cell CJK fonts
            #   BAD:PERF: you can't pass "boxw" inof "lim()" here
            stdscr.addstr(chunk, cattr)
        # ALT:(len(l)>boxw):FAIL: doesn't work due to term-ANSI
        cropped = boff != len(kw["text"])
        return cattr, cropped

    def _render_decortail(
        self,
        stdscr: C.window,
        **infoctx: int | None,
    ) -> None:
        lastline: bool = infoctx.get("lastline")

        # ALT? hide decortail {if not focused} for cleaner !tmux screen-copy
        # MAYBE:PERF: insert "⬎" by item.struct() once inof repeated checks during rendering
        #   FAIL: substruct should be *copyable*, so decortail can't be part of item.struct()
        #   ALSO:BAD: to apply different hi for decortail -- we shouldn't include it into line body
        #
        # SUM: calculate appropriate leading/trailing decortail symbols
        #   ENH: prepend leading "‥" when "offx>0"
        #   ENH: "…" to 1st line when "offs>0"
        #   ENH: "…" as stem into mid/beg of compressed line
        #     │ for smart-compression "…" may also appear in the middle of last/each line,
        #     │ or even replace several lines by two "…" e.g. "…[snippet]…" OR "line1…\n…lineN"
        #   ENH: hi leading/trailing spaces as "·" (nbsp="␣") and tabs as "▸ " (like vim)
        #     << stick them to the text, never overlay onto column-separator
        if infoctx.get("cropped"):
            # BET: always print c-tail over spacer bw columns
            # ALT: print compression "tail" over last char in total "line+tail"
            # FIXME: "…" should be only used if item.struct() had cropped item,
            #   orse if item doesn't fit into navi viewport -- it should always use "‥"
            tail = "…" if lastline else "‥"
        else:
            # MAYBE: print wraps over columns-spacer too (FIXED: tail^=" "*(avail-1))
            # REVL:NICE: current cursor has got behavior of dynamic left-right justifying
            tail = "" if lastline else "⬎" if infoctx.get("newl") else "↩"
        if tail:
            ## BAD? cursor-highlight over column-spacer is very distracting
            cattr = S.iteminfo  # | (S.cursor if infoctx.get("focused") else 0)
            stdscr.addstr(tail, cattr)

    # DECI: pass XY to redraw/render ? OR store as .origin ?
    #   ~store~ makes it possible to .redraw() individual elements only
    #     BAD: all XY should be updated each time we scroll :(
    def render_curses(
        self,
        stdscr: C.window,
        pool: "EntityViewCachePool",
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
        focused = infoctx.get("focusid") is not None

        smattr, smchar = _pick_spacermark(pool, self._ent)
        decortail_len: Final = 1
        boxend: Final = rect.xw - len(smchar)

        def lim() -> int:
            ncells = boxend - stdscr.getyx()[1]
            assert ncells >= 0
            return ncells

        bodyx = rect.x
        if lim() > 21:
            bodyx = self._render_itemnum_prefix(stdscr, lim, **infoctx)
        boxw = boxend - bodyx

        # INFO:(rect.h): "self._item_maxheight_hint" is only a recommendation
        #   >> real limitator is either viewport vh
        #     (in which case we crop item, keeping "⬎" inof "…" on last line),
        #   or we adhere to *policy* of individual item on its current maxheight (OR=item.struct.maxlines)
        #     (with "…" char on last line to indicate "end of item")

        # IDEA: don't always wrap unconditionally on "wrapwidth" -- use nowrap for narrow windows {vw<10}
        #   OPT:BET: allow to pan such cropped name left/right manually (OR by "running line")
        lines = self.struct(
            # INFO: we can pass {boxw>vw}, enabling horizontal panning, marked by "‥"
            wrapwidth=(boxw if boxw > 10 else 0),
            maxlines=(ih_hint if boxw > 10 else 1),
            wrapreserve=1,
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
        last = len(lines) - 1
        for i, l in enumerate(lines):
            lastline = i == last
            stdscr.move(rect.y + i, bodyx)
            if newl := l.endswith(os.linesep):
                l = l.rstrip(os.linesep)
            assert all(c not in l for c in "\r\n"), "TEMP: sub() or repr() embedded NLs"

            # DEBUG: log.info(f"{focused=} | {l=}")
            cattr, cropped = self._render_ansi(
                stdscr,
                ent=self._ent,
                text=l,
                # TODO:(wrapreserve): use {boxw=lim()} for last (or only) line
                # maxcells=(boxw if lastline else boxw - decortail_len),
                maxcells=boxw - decortail_len,
                focused=focused,
            )

            # NOTE: "colored_ansi_or_schema" should ensure all chunks totally fit
            # ATT: we crop to prevent "lim()" from ever becoming negative
            #   TBD: assert() : only applicable to the last chunk of several, orse ivts were broken
            if (nfill := lim()) < 0:
                raise RuntimeError(nfill)

            # NOTE:(last): multiline items are always shortened for nice blocky look
            if last > 0 or not lastline or cropped:
                nfill -= decortail_len

            if focused and nfill:
                # HACK: make cursor span to full viewport width (minus decortail),
                #   colored same as the last chunk
                stdscr.addstr(" " * nfill, cattr | S.cursor)

            if not lastline or cropped:
                self._render_decortail(
                    stdscr,
                    lastline=lastline,
                    cropped=cropped,
                    newl=newl,
                    focused=focused,
                )

        # NOTE: spacermark may be drawn *twice* -- once as charmarker/framecolor for `ItemWidget,
        #   and once again outside item boundaries in `Navi as e.g. a bundle of graph-edges in OpenGL
        if smchar:
            # INFO: we always draw spacermark in top-right corner of visible part of multiline item
            stdscr.addstr(rect.y, rect.xw - 1, smchar, smattr)

        return bodyx  # NOTE: textbody position (to place cursor there)
