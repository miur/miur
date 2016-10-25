#!/usr/bin/python

import os
import curses


entries = os.listdir(os.getcwd())


def main(stdscr):
    # Clear screen
    stdscr.clear()

    begin_x = 20
    begin_y = 7
    height = 5
    width = 40
    win = curses.newwin(height, width, begin_y, begin_x)

    curses.init_pair(1, curses.COLOR_RED, curses.COLOR_WHITE)
    for i, e in enumerate(entries):
        stdscr.addstr(i, 0, e, curses.color_pair(1))

    stdscr.refresh()
    stdscr.getkey()

if __name__ == '__main__':
    curses.wrapper(main)
