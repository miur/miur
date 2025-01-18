from typing import Iterable


def flowratio_to_abs(ratio: Iterable[int | float], maxw: int) -> list[int]:
    """
    Convert fixed/flexed width hints to abs width fitting into maxw
    LIOR? int=Fixed/Column=-min/max, float=Percent/Ratio=-rest/full
    """
    ratio = tuple(ratio)
    assert ratio and maxw > 0
    assert all(r > -1 for r in ratio), "TEMP:DECI: meaning of negative int"
    wlst = [int(r * maxw) if 0 < r < 1 else int(r) if r >= 1 else 0 for r in ratio]

    fixed = sum(w for w in wlst if w > 0)
    if fixed > maxw:
        raise ValueError((maxw, ratio))

    flexed = maxw - fixed
    for i, r in enumerate(ratio):
        if r < 0:
            wlst[i] = int(-r * flexed)
    filled = sum(wlst)
    if filled > maxw:
        raise ValueError((flexed, ratio))

    spaced = maxw - filled
    nr_spaced = sum(1 for w in ratio if w == 0)
    if nr_spaced > 0:
        if spaced <= 0:
            raise ValueError((spaced, ratio))
        for i, r in enumerate(ratio):
            if r == 0:
                wlst[i] = spaced // nr_spaced
    residue = maxw - sum(wlst)
    if residue != 0:
        raise ValueError((residue, ratio))
    assert wlst
    assert all(w >= 2 for w in wlst), "ERR: resulting column is too narrow"
    return wlst
