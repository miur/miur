import os
from typing import TYPE_CHECKING, Final

import _curses as C

from ..curses_ext import g_style as S
from ..util.termansi import ChunkKind as K
from ..util.termansi import cellchunk, cellwidth, num_lo, num_up
from . import itemparts as P

# from ..util.logger import log
from .entity_base import Golden
from .rect import Rect

if TYPE_CHECKING:
    from .navihistory import EntityViewCachePool

hint_numcol_len: Final = 5
hint_minbody_len: Final = 8  # CASE: only when prefix is shown
hint_mintext_len: Final = 4  # CASE: only when meta is shown; orse =1
decortail_len: Final = 1
hint_minboxw: Final = hint_numcol_len + hint_minbody_len + decortail_len  # =14


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
    # MOVE? make ItemWidget to calc item height and draw it (or only ItemXfm) inof directly drawing Item/Entity
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
        text = self._ent.name.replace("o", "o" + NL).replace("\t", "▸ ")
        # ALT:(splitlines):FAIL: discards empty lines
        lines = text.replace(NL, NL + NUL).split(NUL)
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
        # lines: list[str] = []
        # s = self._ent.name
        # c = 0
        # # VIZ: separately(maxwrap+maxnewlines), combined(maxwrap<maxlines), unlimited
        # while c < len(s) and len(lines) <= maxlines:
        #     nc = min(c + wrapwidth, len(s) - c)
        #     if (nn := s.find("\n", c, c + wrapwidth)) >= 0:
        #         nc = min(nn, nc)
        #     lines.append(s[c:nc])
        #     c = nc
        return lines

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
        numcol: bool = False,
        **infoctx: int | None,
    ) -> int:
        focused = infoctx.get("focusid") is not None
        moreup = infoctx.get("moreup")
        moredown = infoctx.get("moredown")

        maxlines = min(rect.h, ih_hint if rect.w > 10 else 2)  # , self.height())
        cursorx = rect.x

        text = self._ent.name.replace("o", "o\n")
        kind, cw, ti = K.partial, 0, 0

        view = pool.get(self._ent)
        smattr, smchar = P.pick_spacermark(view)
        # TEMP: only draw metainfo for cached nodes
        meta = str(len(view._orig_lst)) if view else ""

        # INFO:(rect.h): "self._item_maxheight_hint" is only a recommendation
        #   >> real limitator is either viewport vh
        #     (in which case we crop item, keeping "⬎" inof "…" on last line),
        #   or we adhere to *policy* of individual item on its current maxheight (OR=item.struct.maxlines)
        #     (with "…" char on last line to indicate "end of item")

        # IDEA: don't always wrap unconditionally on "wrapwidth" -- use nowrap for narrow windows {vw<10}
        #   OPT:BET: allow to pan such cropped name left/right manually (OR by "running line")
        # ENH: allow wrapping each line on different length (based on cellwidth(meta[i]))
        #   MAYBE:ALSO:TEMP: do wrapreserve=1 for this unconditionally
        # BET?:API: self.chunk(maxw, start=...) to both reflow/split
        # lines = self.struct(
        #     # INFO: we can pass {boxw>vw}, enabling horizontal panning, marked by "‥"
        #     wrapwidth=(boxw if boxw > 10 else 0),
        #     maxlines=(ih_hint if boxw > 10 else 1),
        #     wrapreserve=decortail_len,
        # )
        # log.trace(lines)  # <DEBUG:(line split/wrap)

        # PERF~BAD? replace by pre-advanced iterator to avoid unnecessary copying
        #   https://stackoverflow.com/questions/11383468/python-iterate-over-a-sublist
        # ALT: insert into loop {if i > rect.h or i < offy: continue; i-=offy}
        # if offy:
        #     assert 0 <= offy < len(lines)
        #     lines = lines[offy:]
        # if len(lines) > rect.h:
        #     lines = lines[: rect.h]

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

        stdscr.move(rect.y, rect.x)

        # last = len(lines) - 1
        # for i, l in enumerate(lines):
        last = maxlines - 1
        assert 0 <= offy <= last
        i = -offy - 1
        # TODO: combine with item.struct() linewrap to split chunks and dup cattr on wrapwidth
        while (i := i + 1) <= last:
            # {if i > rect.h or i < offy: continue; i-=offy}
            # FIXME:BET:(stable reflow): don't draw numcol {if i < offy}
            if i == 0:
                ## FMT:0: rect.w = [numcol + boxw(bodyw(text + roomw/nfill?) + decortail? + meta?)] + smchar?
                ## FMT:N: rect.w = [indent + boxw(bodyw + decortail?)]
                ##   * &prio: (bodyw==smchar) > decortail > numcol > meta
                ##   * TODO: skip decortail_len=1 if meta and not decortail
                ##   * TODO: skip decortail_len=1 if i==last and not colsep and "bodyx+boxw"==rect.xw
                ##       &why to have visual gap bw columns when colsep=""
                ##   RENAME? boxw -> ncells
                boxw = rect.w - cellwidth(smchar)
                if numcol and boxw > hint_minboxw:
                    bodyx = P.render_itemnum_prefix(stdscr, self._ent, boxw, **infoctx)
                    cursorx = bodyx
                else:
                    bodyx = rect.x
                boxw -= bodyx - rect.x
                # NOTE: reserve space for metainfo at the end of *first* line-part (OR: *each*)
                if (namew := boxw - cellwidth(meta)) >= hint_mintext_len:
                    bodyw = namew
                else:
                    meta = ""
                    bodyw = boxw
            else:
                indent = 2 if cursorx > rect.x else 1
                bodyx = rect.x + indent
                boxw = rect.w - (bodyx - rect.x)
                bodyw = boxw

            # [_] XP~TRY: wrap by max(30,roomw), and then crop into actual roomw :NEED:(K.cropped)
            #   &why NICE: less noisy presentation in narrow columns for more items
            # NOTE: multiline items are always shortened for nice blocky look
            #       and to have visual gap bw columns when colsep=""
            roomw = bodyw - decortail_len
            assert roomw > 1
            stdscr.move(rect.y + i, bodyx)
            while roomw > 0 and ti < len(text) and kind not in (K.charwrap, K.NL):
                kind, cw, te = cellchunk(text, roomw, ti)
                if kind == K.TAB:
                    chunk, cattr = "▸ ", S.iteminfo
                elif kind == K.NUL:
                    chunk, cattr = "␀", S.error
                elif kind == K.DEL:
                    chunk, cattr = "␡", S.error
                elif kind == K.ctrl:
                    chunk = "^" + chr(ord("A") + ord(text[ti:te]))
                    cattr = S.error
                else:
                    chunk = text[ti:te]
                    cattr = S.default  # =FIXME: =resolve_colorscheme(ent)
                stdscr.addstr(chunk, cattr | (S.cursor if focused else 0))
                roomw -= cw
                ti = te

            # DEBUG: log.info(f"{focused=} | {l=}")
            # if newl := l.endswith(os.linesep):
            #     l = l.rstrip(os.linesep)
            # assert all(c not in l for c in "\r\n"), "TEMP: sub() or repr() embedded NLs"
            # NOTE: "colored_ansi_or_schema" should ensure all chunks totally fit bodyw
            # cattr, cropped, bodyend = P.render_ansi(
            #     stdscr,
            #     ent=self._ent,
            #     text=l,
            #     # TODO:(wrapreserve): use {boxw=lim()} for last (or only) line
            #     # maxcells=(boxw if lastline else boxw - decortail_len),
            #     maxcells=boxw - decortail_len,
            # )

            if roomw < 0:
                raise RuntimeError(roomw)

            # HACK: skip upper lines of half-shown item
            # BET?(don't draw numcol at all): if i < offy: continue
            if i < 0:
                continue

            if focused and roomw:
                # HACK: make cursor span to full viewport width (minus decortail),
                #   colored same as the last chunk
                stdscr.addstr(" " * roomw, cattr | S.cursor)

            # NICE: don't override decortail, to indicate if line was wrapping or not
            if kind != K.end:
                P.render_decortail(
                    stdscr,
                    # FIXME! lastline of half-visible bot item needs decortail=wrap,newl
                    lastline=i == last,
                    cropped=ti < len(text),  # CHG:SEE:NEED:(K.cropped)
                    newl=kind == K.NL,
                    focused=focused,
                )

            ## BAD: other kinds of morechar overlay
            #  * char0: feels as if belongs to *prev* column; messes text readability
            #  * decortail: feels like it belongs to *next* column
            #  * append: distractingly changes position on scroll due to varying cellwidth(name)
            #  * midchar: "always there, always shown", which distracts from reading actual name
            #    IDEA: hide it temporarily during scroll, and show back by timeout after key-release
            #  * BET? put morechar left of decortail, to keep it untouched
            ## XP~ALT: put hasmore into "meta" inof doing overlay
            ##   OR put new "element" bw meta and smchar, and reflow bodyw
            if moreup and i == 0:
                ms = "△" + num_up(moreup)  # ALT="△⁽¹²³⁾"
                mn = cellwidth(ms)
                ms = ms if boxw > hint_mintext_len + mn else ms[0]
                mx = rect.xw - 1 - mn  # OR=rect.x + rect.w // 2
                stdscr.addstr(rect.y + i, mx, ms, S.empty)
            elif moredown and i == last:
                assert not moreup or last > 0, "FIX: print both up/dn on same line"
                ms = "▽" + num_lo(moredown)  # ALT="▽₍₁₂₃₎"
                mn = cellwidth(ms)
                ms = ms if boxw > hint_mintext_len + mn else ms[0]
                mx = rect.xw - 1 - mn  # OR=rect.x + rect.w // 2
                stdscr.addstr(rect.y + i, mx, ms, S.empty)

            if i == 0:
                if meta:
                    if not focused and roomw:
                        stdscr.addstr(" " * roomw, cattr)
                    # ALT: print "meta" on the last linepart, where item more likely to have empty space
                    stdscr.addstr(meta, S.auxinfo)
                # NOTE: spacermark may be drawn *twice* -- once as charmarker/framecolor for `ItemWidget,
                #   and once again outside item boundaries in `Navi as e.g. a bundle of graph-edges in OpenGL
                if smchar:
                    # INFO: we always draw spacermark in top-right corner of visible part of multiline item
                    stdscr.addstr(rect.y + i, rect.xw - 1, smchar, smattr)

        return cursorx  # NOTE: textbody position (to place cursor there)
