import curses

from miur.cursor import state


def draw(stdscr):
    curs = state.cursor
    lines = state.entries
    if curs is None or lines is None:
        return

    x, y = 1, 0
    h, w = stdscr.getmaxyx()

    st = int(h/2)
    sb = int(h/2)

    if len(lines) < h:
        top = 0
    elif curs < st:
        top = 0
    elif curs > len(lines) - sb - 1:
        top = len(lines) - h
    else:
        top = curs - st

    # FIXME: slice is slow on large set
    vlst = lines[top:(top + h)]
    vpos = curs - top

    for i, e in enumerate(vlst):
        yt = y + i
        xt = x
        rx = w - xt

        # WARN: don't call addstr at all if there is no place even for 1 char
        if rx > 0:
            if rx < len(e):
                e = e[:rx]

            # ALT:(faster) draw whole list -- then re-draw line for cursor
            #   => BUT: if we choose individual colors -- no sense in optimization
            if i == vpos:
                stdscr.addstr(yt, xt, e, curses.color_pair(2),)
            else:
                stdscr.addstr(yt, xt, e)

    stdscr.move(vpos, 0)
