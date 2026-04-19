#
# SPDX-FileCopyrightText: 2016 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
import curses

from .tui import draw
from .keymap import keymap
from miur.cursor import update


def prepare(stdscr):
    # begin_x = 20
    # begin_y = 7
    # height = 5
    # width = 40
    # curses.newwin(height, width, begin_y, begin_x)

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
