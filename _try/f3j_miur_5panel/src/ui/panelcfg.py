from .panel import Panel
from .rect import Rect


def thisfnm() -> str:
    import sys

    # OR: here = inspect.currentframe()
    here = sys._getframe()  # pylint:disable=protected-access
    caller = here.f_back
    assert caller
    return caller.f_code.co_name


class PanelCfg:
    def __init__(self, sepw: int) -> None:
        self.sepw = sepw

    def browse(self, w: float = 0, s: int = 0) -> Panel:
        return Panel(thisfnm(), [Panel("tab0")], w=w, sepw=s)

    def preview(self, w: int = 0, s: int = 0) -> Panel:
        return Panel(thisfnm(), [Panel("pv0")], w=w, sepw=s)

    def preview2(self, w: float = 0, s: int = 0) -> Panel:
        return Panel("preview", [Panel("pv0", w=w), Panel("pv1")], sepw=s)

    def prevloci2(self, w: int = 0, s: int = 0) -> Panel:
        return Panel("prevloci", [Panel("pprev", w=0.4), Panel("prev")], w=w, sepw=s)

    def navi_vlst(self) -> Panel:
        return Panel(thisfnm(), [self.browse()])

    def navi_pv0(self) -> Panel:
        return Panel(thisfnm(), [self.browse(), self.preview(12)])

    def navi_miller0(self) -> Panel:
        lst = [Panel("prevloci", [Panel("prev")], w=8), self.browse(), self.preview(12)]
        return Panel(thisfnm(), lst, sepw=self.sepw)

    def navi_miller1(self) -> Panel:
        s = self.sepw
        prevloci = Panel("prevloci", [Panel("prev")], w=16, sepw=s)
        lst = [prevloci, self.browse(0.5, s), self.preview(s=s)]
        return Panel(thisfnm(), lst, sepw=s)

    def navi_miller2(self) -> Panel:
        s = self.sepw
        ## ADD? "interp" bw "browse" and "preview"
        # interp = PanelView(ratio=(0,))
        lst = [self.prevloci2(22, s), self.browse(0.5, s), self.preview2(0.7, s)]
        # TODO:ALSO: return linewrap/header/footer cfg overrides
        return Panel(thisfnm(), lst, sepw=s)

    # CASE: disable previews to better read current long lines in e.g. `Exception or `TextCode
    #   BUT: keep two-level history to see both originator Entity and its Action
    def navi_hist2(self) -> Panel:
        s = self.sepw
        return Panel(thisfnm(), [self.prevloci2(22, s), self.browse(s=s)], sepw=s)

    # NOTE:(`AdaptiveLayout): change/hide based on total window size
    #   DFL:(prio): browser:0 -> preview:1 -> parent:2 [-> pparent:3]
    def navi_adaptive(self, rect: Rect, old: Panel) -> Panel:
        if rect.w < 30:
            fl = self.navi_vlst
        elif rect.w < 45:
            fl = self.navi_pv0
        elif rect.w < 60:
            fl = self.navi_miller0
        elif rect.w < 70:
            fl = self.navi_miller1
        else:
            fl = self.navi_miller2

        if old.name == fl.__name__:
            return old
        return fl()
