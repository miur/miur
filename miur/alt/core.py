import curses


def run(argv):
    dom = Dom(numbers)
    proxy = Proxy(dom.data)
    view = View(proxy.data)
    print(view)
    # print(argv)
    # state = {'cursor': 0, 'entries': None}
    # curses.wrapper(loop, state)


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
    return iter(range(1, 20))


class Dom(object):
    def __init__(self, accessor):
        self._accessor = accessor

    def __str__(self):
        return '\n'.join(str(i) for i in self.data())

    # TEMP:(assume list): get whole data
    def data(self):
        return self._accessor()


# rename => View
# VIZ. DataProxy, ListProxy, MatrixProxy, ScalarProxy, DictProxy, TableProxy, RawProxy, ImageProxy
class Proxy(object):
    def __init__(self, accessor):
        self._accessor = accessor

    def __str__(self):
        return '\n'.join(str(i) for i in self.data())

    # TEMP:(assume list):
    def data(self):
        return list(reversed(sorted(self._accessor())))[4:]


# rename => Cursor/Focus/Slice
class View(object):
    def __init__(self, accessor):
        self._accessor = accessor
        # TEMP:(cursor==index): hardcoded relation
        #   on delete file before cursor => decrease index
        self.cursor = 3  # EXPL:(None): only on empty proxy
        # WTF:NEED: cursor per each node

    def __str__(self):
        return '\n'.join((str(v) if i != self.cursor else str(v) + ' **')
                         for i, v in enumerate(self.data(None, None)[1]))

    def data(self, h, w):
        return self.cursor, self._accessor()


# NEED: notion of 'focus', 'cursor', 'groups' => THINK decorated widgets (? how to composite ?)
# focus  :: dim unfocused widget, generate 'dying out' hi tail in list on cursor movement
# cursor :: 'bubble cursor' uses more then single line of output, pushing apart top and bottom pieces
# groups :: horiz delims between groups of files => special treatment, special fetcher to know where to place them
# baking :: compact path names to fit into widget size
class Widget(object):
    def __init__(self, accessor):
        self._accessor = accessor

    def __str__(self):
        return '\n'.join((str(v) if i != self.cursor else str(v) + ' **')
                         for i, v in enumerate(self.data(None, None)[1]))

    def data(self, h, w):
        curs, elems = self._accessor(h, w)


# VIZ. horizontal, vertical, grid, etc
class Layout(object):
    def __init__(self, widget):
        self._widget = widget
