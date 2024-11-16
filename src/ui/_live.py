from ..util.exchook import log_exc
from ..util.logger import log
from .entries import FSEntry
from .root import RootWidget

# from . import widget as this  # pylint:disable=import-self


def _live() -> None:
    log.sep()
    from ..app import g_app as g

    # pylint:disable=protected-access
    # HACK: we should restore all "EntryView" contexts, as we recreate ALL objects
    #   ENH:TODO: serialize/deserialize whole `Views inof only current view pos/idx ?
    pent = FSEntry("/d/airy")
    hint_idx = 0
    hint_pos = 0
    if pnavi := getattr(g.root_wdg, "_navi", None):
        pent = pnavi._view._ent
        hint_idx = pnavi._view._wdg._cursor_item_lstindex
        hint_pos = pnavi._view._wdg._viewport_followeditem_linesfromtop

    # i: int
    try:
        g.root_wdg = RootWidget(pent)
        # HACK:(this.*): force cmp with new instance after reload()
        # g.root_wdg.set_entity(this.FSEntry("/etc/udev"), hint_idx=hidx)
        g.curses_ui.resize()

        if hint_idx is not None:
            g.root_wdg._navi._view._wdg._cursor_item_lstindex = hint_idx
            g.root_wdg._navi._view._wdg._viewport_followeditem_lstindex = hint_idx
            # FIXME: recalc(hint_pos) if resize() happened meanwhile
            g.root_wdg._navi._view._wdg._viewport_followeditem_linesfromtop = hint_pos

        g.root_wdg.redraw(g.stdscr)
        g.stdscr.refresh()

        # ALT: fuzzy-test, by random direction of up/down 50 times
        # ndown = 30
        # for i in range(ndown):
        #     wdg.cursor_step_by(1)
        # wdg.redraw(g.stdscr)
        # g.stdscr.refresh()
        # for i in range(ndown):
        #     wdg.cursor_step_by(-1)
        # wdg.redraw(g.stdscr)
        # g.stdscr.refresh()
    except Exception as exc:  # pylint:disable=broad-exception-caught
        # exc.add_note(f"{i=}")
        log_exc(exc)
