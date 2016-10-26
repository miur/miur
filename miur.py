#!/usr/bin/python

import os
import curses


from miur import dom


def draw(scrn):
    for i, e in enumerate(dom.entries):
        scrn.addstr(i, 0, e, curses.color_pair(1))
    scrn.refresh()


def main(scrn):
    # Clear screen
    scrn.clear()

    begin_x = 20
    begin_y = 7
    height = 5
    width = 40
    curses.newwin(height, width, begin_y, begin_x)

    curses.init_pair(1, curses.COLOR_RED, curses.COLOR_WHITE)

    draw(scrn)

    scrn.getkey()


def comm():
    from miur.relay.client import send
    from miur.relay.server import Listener

    # BAD: not deleted afterwards
    # with Listener('/tmp/miur') as l:
    with Listener(("localhost", 0)) as l:
        saddr = l.server.server_address

        send(saddr, os.listdir(os.getcwd()))
        send(saddr, os.listdir('/tmp'))


if __name__ == '__main__':
    comm()
    curses.wrapper(main)
