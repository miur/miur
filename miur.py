#!/usr/bin/python

import curses


def main(stdscr):
    # Clear screen
    stdscr.clear()

    begin_x = 20
    begin_y = 7
    height = 5
    width = 40
    win = curses.newwin(height, width, begin_y, begin_x)

    stdscr.refresh()
    stdscr.getkey()

if __name__ == '__main__':
    curses.wrapper(main)
