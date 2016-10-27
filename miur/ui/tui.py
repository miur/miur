import curses

from miur.ui import frame


def draw(stdscr):
    for i, e in enumerate(frame.entries):
        stdscr.addstr(i, 0, e)
    stdscr.refresh()


def loop(stdscr):
    stdscr.clear()

    # begin_x = 20
    # begin_y = 7
    # height = 5
    # width = 40
    # curses.newwin(height, width, begin_y, begin_x)

    curses.init_pair(1, curses.COLOR_RED, curses.COLOR_WHITE)
    stdscr.attron(curses.color_pair(1))

    stdscr.nodelay(False)
    nd = 0
    while True:
        c = stdscr.getch()
        if c == ord('n'):
            ++nd
            if nd >= len(frame.dirs):
                nd = 0
            frame.update(frame.dirs[nd])
        elif c == ord('q') or c == curses.KEY_ENTER:
            break  # Exit the while loop
        draw(stdscr)


def main(saddr):
    frame.saddr = saddr
    curses.wrapper(loop)
