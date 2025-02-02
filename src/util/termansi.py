import enum
from unicodedata import east_asian_width

NUM_SUP = "⁰¹²³⁴⁵⁶⁷⁸⁹"  # ⁺⁻⁼⁽⁾ⁱⁿ
NUM_sub = "₀₁₂₃₄₅₆₇₈₉"  # ₊₋₌₍₎ᵢⱼₖᵣₜᵥₓ


def _num_map(x: int | str, c2u: str) -> str:
    if not isinstance(x, str):
        x = str(x)
    # else:
    #     assert x.isdigit()
    return "".join(c2u[int(c)] if c.isdigit() else c for c in x)


def num_lo(x: int | str) -> str:
    return _num_map(x, NUM_sub)


def num_up(x: int | str) -> str:
    return _num_map(x, NUM_SUP)


# FIXED:BAD:PERF: curses doesn't understand multi-cell CJK fonts
def cellwidth(text: str) -> int:
    if not text:
        return 0
    if text.isascii():
        return len(text)
    # BAD: actual char width may depend on !st.render and !font (chosen by !fontconfig)
    #   >> some chars have triple-cell width, and some fonts fit wide chars in single cell
    return len(text) + sum(1 for wch in text if east_asian_width(wch) in "WF")


# HACK: categorize how dif. chunks should be displayed dif.ly
class ChunkKind(enum.Enum):
    NL = ""  # OR="\n"/os.linesep
    NUL = "␀"  # OR=^@
    TAB = "▸ "  # OR="    "  BET?=str.expandtab(4)
    DEL = "␡"  # OR=^?/^~ "\x7f" ALT=curses.KEY_*
    ctrl = "^^"  # chr<0x20
    end = -1  # ATT: we still need to compare ti<len(text) outside
    ## FAIL: linepart may end in special char, so we need to combine it with wrap
    ##   ALT?IDEA: yield empty string after special char, with proper kind
    partial = 0  # RENAME? .part | .hasmore
    charwrap = 1
    # wordwrap = 2  ## TODO: "softwrap" on space/punct word boundaries


##%ONELINE: [ l[c * iw : c * (iw + 1)] + ("↩" if c < maxwrap else "…")
##          for l in item.name.split("\n") for c in range((len(l) // iw) + 1) if c <= maxwrap ]
# API:PERF: don't return (chunk, rest) as copying large strings is slow
def cellchunk(text: str, maxcw: int, start: int = 0) -> tuple[ChunkKind, int, int]:
    assert maxcw >= 0  # NOTE:(=0): allowed for newline and charwrap in-place processing
    ti = start
    ci = 0
    kind: ChunkKind | None = None
    while ti < len(text):
        wch = text[ti]
        wchi = ord(wch)
        if wchi > 0x7F:
            cw = 2 if east_asian_width(wch) in "WF" else 1
        elif wchi >= 0x20 and wchi != 0x7F:
            cw = 1
        else:
            if wch == "\n":  # FIXME?(Windows): should work for os.linesep="\r\n"
                # BAD: currently it always yields .partial and then empty .NL
                #   FIXME: yield linepart with .NL in one go
                kind = ChunkKind.NL
                cw = 0  # COS: we don't print it OR:THINK: we may reserve cw= for decortail?
            elif wch == "\t":
                # NOTE: yield pairs (cattr, chunk) to colorize unprintable symbols differently
                kind = ChunkKind.TAB
            elif wchi == 0:
                kind = ChunkKind.NUL
            elif wchi == 0x7F:
                kind = ChunkKind.DEL
            else:  # wchi < 0x20
                kind = ChunkKind.ctrl
            cw = len(kind.value)

        # WARN: we wrap last *widechar* if remaining space only =1  OR: drop/lose it
        # HACK: yield empty string imm after special char when no room is left (to signify wrapping)
        # XP~HACK: ignore wrapping/chunking if maxcw==-1, and only split by NL, cropping rest
        # if ci + cw > maxcw > -1:
        if ci + cw > maxcw:
            return (ChunkKind.charwrap, ci, ti)
        if ti > start and kind:
            return (ChunkKind.partial, ci, ti)
        ci += cw
        ti += 1
        if kind:
            return (kind, ci, ti)
    return (ChunkKind.end, ci, ti)
