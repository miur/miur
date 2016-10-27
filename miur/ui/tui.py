import curses

from miur.ui import frame


def draw(scrn):
    for i, e in enumerate(frame.entries):
        scrn.addstr(i, 0, e, curses.color_pair(1))
    scrn.refresh()


def loop(scrn):
    scrn.clear()

    begin_x = 20
    begin_y = 7
    height = 5
    width = 40
    curses.newwin(height, width, begin_y, begin_x)

    curses.init_pair(1, curses.COLOR_RED, curses.COLOR_WHITE)

    draw(scrn)

    scrn.getkey()


def main():
    curses.wrapper(loop)
