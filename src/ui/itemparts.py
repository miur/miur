# RENAME?(itemparts.py): -> item_elements.py , item_pieces.py , ./item/*.py
from typing import TYPE_CHECKING, Any, Iterable

import _curses as C

# from ..util.logger import log
from ..entity.base import Golden
from ..entity.error import ErrorEntry
from ..entity.text import TextEntry
from .colorscheme import g_style as S
from .itemcolor import colored_ansi_or_schema

if TYPE_CHECKING:
    from .view import EntityView


# TODO:OPT: number-column variants:
#   * [rel]linenum
#   * [rel]viewpos
#   * [rel]itemidx
#   * combined=itemidx+[rel]viewpos
def pick_itemnum_prefix(
    ent: Golden, boxw: int, **infoctx: int | None
) -> Iterable[tuple[int, str]]:
    focused = infoctx.get("focusid") is not None
    combined = True  # OPT:TEMP: hardcode
    if combined:
        cidx = infoctx.get("lstidx")
        bidx = infoctx.get("vpidx")
        if isinstance(ent, TextEntry):
            if "`0x" in ent.loci:
                bpfx = cpfx = "" if cidx is None else f"{cidx*16:02x}"
            else:
                bpfx = cpfx = "" if cidx is None else f"{1+cidx:02d}"
        else:  # e.g. FSEntry, etc.
            cpfx = "" if cidx is None else f"{1+cidx:02d}"
            bpfx = "" if bidx is None else f"{1+bidx:02d}"

        if focused:
            if cpfx:
                yield (S.pfxrel, f"{cpfx:>3s}> ")
        else:
            if bpfx:
                yield (S.pfxidx, f"{bpfx:>3s}: ")
    else:
        # HACK: hide both numcol when viewport is too small
        if boxw > 25 and (vpidx := infoctx.get("vpidx")) is not None:
            pfxvp = f"{1+vpidx:02d}| "
            yield (S.pfxrel, pfxvp)
        # TODO: for binary/hex show "file offset in hex" inof "item idx in _xfm_list"
        #   ALSO: start offsets from "0x0" inof "1"
        if boxw > 21 and (lstidx := infoctx.get("lstidx")) is not None:
            # IDEA: shorten long numbers >999 to ‥33 i.e. last digits significant for column
            #   (and only print cursor line with full index)
            pfxlst = f"{1+lstidx:03d}{">" if focused else ":"} "
            yield (S.pfxidx, pfxlst)


def render_ansi(
    stdscr: C.window, focused: bool = False, **kw: Any
) -> tuple[int, bool, int]:
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
        stdscr.addstr(chunk, cattr | (S.cursor if focused else 0))
    # ALT:(len(l)>boxw):FAIL: doesn't work due to term-ANSI
    cropped = boff != len(kw["text"])
    return cattr, cropped, stdscr.getyx()[1]


def render_decortail(stdscr: C.window, **infoctx: bool) -> None:
    lastline = infoctx.get("lastline")

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
        # SUM: using "‥" at the end of each linepart, which longer than viewport
        #   [_] OR:DEV: smart-compress with "‥" in the middle each part of .name bw newlines
        # XP~MAYBE: put combined "⬎ and …" if newl -- to indicate cropping by maxh (inof only maxw)
        tail = "…" if lastline else "‥"
    else:
        # MAYBE: print wraps over columns-spacer too (FIXED: tail^=" "*(avail-1))
        # REVL:NICE: current cursor has got behavior of dynamic left-right justifying
        tail = "" if lastline else "⬎" if infoctx.get("newl") else "↩"
    if tail:
        ## BAD? cursor-highlight over column-spacer is very distracting
        #    TRY: recombine decortail fg with cursor rev/bg to blend colors
        cattr = S.iteminfo  # | (S.cursor if infoctx.get("focused") else 0)
        stdscr.addstr(tail, cattr)


# RENAME?MOVE? EntityView.classify_preview()
# ALT:PERF: do it only once on vlst.assign() and store inside `ItemWidget
def pick_spacermark(v: "EntityView | None") -> tuple[int, str]:
    ## NOTE: preview-hint to be aware of next steps w/o moving cursor
    ## BAD: only shows marker for already cached views, so you NEED to move cursor
    ##   FIXME? gen-preview for all items in list -- BUT:PERF
    # BET:PERF: cache weak fwd-ref to corresponding `EntityView inside `ItemWidget
    # MAYBE:ARCH: access directly to global "g_app.entvpool"
    # pylint:disable=protected-access
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
