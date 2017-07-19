import curses


def run(argv):
    print(argv)
    state = {'cursor': 0, 'entries': None}
    curses.wrapper(loop, state)


def prepare(stdscr):
    curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLACK)
    curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_CYAN)
    stdscr.attron(curses.color_pair(1))
    stdscr.nodelay(False)


def loop(stdscr, state):
    prepare(stdscr)
    while True:
        stdscr.clear()
        draw(stdscr, state)
        stdscr.refresh()
        # key = stdscr.getkey()


def draw(stdscr, state):
    curs, lines = state['cursor'], state['entries']
    if curs is None or lines is None:
        return

    x, y = 1, 0
    h, w = stdscr.getmaxyx()

    st = int(h/2)
    sb = int(h/2)

    # DEV: sep func 'crop list by area'
    # DEV: sep func 'draw list in area'
    #   ? how to do it bias-independent (~glPushMatrix()~)

    # WTF:(crash): list redraw when screen is shrinking
    #   ? check screen height before each line draw ?
    #   FIND: if ncurses supports 'virtual canvas' to draw on
    #   FIND: how ncurses treats content when screen shrinks -- cuts it w/o restoring ?
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

            # ALT:(faster) draw whole list -- then redraw line for cursor
            #   => BUT: if we choose individual colors -- no sense in optimization
            if i == vpos:
                stdscr.addstr(yt, xt, e, curses.color_pair(2),)
            else:
                stdscr.addstr(yt, xt, e)

    stdscr.move(vpos, 0)


# NOTE: produce view cache
def numbers():
    return iter(range(1, 100))


# VIZ. ListProxy, MatrixProxy, ScalarProxy, DictProxy, TableProxy, RawProxy, ImageProxy
class DataProxy(object):
    def __init__(self, accessor):
        self._accessor = accessor

    def cut(self, h, w):
        return self._accessor(h, w)[:h]


# NEED: notion of 'focus', 'cursor', 'groups' => THINK decorated widgets (? how to composite ?)
# focus  :: dim unfocused widget, generate 'dying out' hi tail in list on cursor movement
# cursor :: 'bubble cursor' uses more then single line of output, pushing apart top and bottom pieces
# groups :: horiz delims between groups of files => special treatment, special fetcher to know where to place them
class Widget(object):
    def __init__(self, dataproxy):
        self._dataproxy = dataproxy

    def bake(self, h, w):
        self._dataproxy.cut(h, w)


# VIZ. horizontal, vertical, grid, etc
class Layout(object):
    pass
