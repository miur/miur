import curses


def run(argv):
    print(argv)

    curses.wrapper(loop)


def prepare(stdscr):
    curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLACK)
    curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_CYAN)
    stdscr.attron(curses.color_pair(1))
    stdscr.nodelay(False)


def loop(stdscr):
    prepare(stdscr)
    while True:
        stdscr.clear()
        draw(stdscr)
        stdscr.refresh()
        key = stdscr.getkey()
