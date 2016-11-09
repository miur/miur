import curses

from .keymap import keymap
from miur.cursor import graph, state, update


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


def prepare(stdscr):
    # begin_x = 20
    # begin_y = 7
    # height = 5
    # width = 40
    # curses.newwin(height, width, begin_y, begin_x)

    curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLACK)
    curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_CYAN)
    stdscr.attron(curses.color_pair(1))


def loop(stdscr):
    prepare(stdscr)
    stdscr.nodelay(False)
    while True:
        stdscr.clear()
        draw(stdscr)
        stdscr.refresh()
        # WARN:NEED:(coro) if socket blocks -- ui will block too
        # DEV: send cmd to socket, get list to show
        # THINK: use non-blocking input in single-thread asyncio coroutine ?
        key = stdscr.getkey()
        # Search user mapping
        cmd = keymap.get(key, None)
        if cmd is None:
            continue
        ret = update.update(cmd)
        # TEMP:HACK:(asymmetrical) quit
        if ret is not None:
            break


def main(server_address):
    # EXPL: Init first screen (WARN: multithreading timings)
    state.entries = graph.list_nodes(state.path)
    curses.wrapper(loop)
