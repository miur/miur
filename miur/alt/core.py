import curses
import logging
_log = logging.getLogger(__name__.split('.', 2)[1])

keymap = {
    'j': 'focus_node_next',
    'k': 'focus_node_prev',
    '^L': 'redraw_all_now',
    'q': 'quit',
    '\n': 'quit',
}


def run(argv):
    dom = Dom(numbers)
    proxy = Proxy(dom.data)
    cursor = Cursor(proxy)
    view = View(proxy.data, cursor)
    widget = Widget(view)

    state = {'keymap': keymap}
    nc_frmwk = NcursesFramework(state)
    nc_front = NcursesFrontend(nc_frmwk, widget)
    nc_input = NcursesInput(nc_frmwk, cursor)
    state['frontend'] = nc_front
    state['input'] = nc_input
    nc_frmwk.run(nc_input.loop)


def send_event(event, state):
    if event == 'redraw':
        # DEV: skip some draw requests to limit fps
        state['frontend'].draw(state)


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


class Cursor(object):
    def __init__(self, proxy):
        self._proxy = proxy
        # TEMP:(cursor==index): hardcoded relation
        #   on delete file before cursor => decrease index
        # WTF:NEED: cursor per each node
        self._index = 3  # EXPL:(None): only on empty list

    def __str__(self):
        return '\n'.join(str(i) for i in self.data())

    def data(self):
        return self._proxy.data()

    def focus_node_next(self):
        if self._index is not None and self.data() is not None:
            self._index = min(self._index + 1, len(self.data()) - 1)

    def focus_node_prev(self):
        if self._index is not None and self.data() is not None:
            self._index = max(self._index - 1, 0)


# rename => Slice / Projection
# NOTE: combines Cursor and Proxy to get Slice
# * acquires certain data type from DOM around cursor
#   +/- entries of list
#   elements of before/after path
# DEV: sep func 'crop list by area'
# DEV: sep func 'draw list in area'
#   ? how to do it bias-independent (~glPushMatrix()~)
#   TRY: impl by curses.newwin(height, width, begin_y, begin_x)
class View(object):
    def __init__(self, accessor, cursor):
        self._accessor = accessor
        self.cursor = cursor

    # DEBUG
    def __str__(self):
        return '\n'.join((str(v) if i != self.cursor._index else str(v) + ' **')
                         for i, v in enumerate(self.data(None, None)))

    # NOTE: evaluate scroll window indexes
    def top(self, h):
        lnum = len(self._accessor())
        cur_pos = self.cursor._index
        thr_low = h // 4
        thr_high = h * 3 // 4
        if lnum < h:
            top = 0
        elif cur_pos < thr_low:
            top = 0
        elif cur_pos > lnum - thr_high - 1:
            top = lnum - h
        else:
            top = cur_pos - thr_low
        return top

    def data(self, h, w):
        top = self.top(h)
        # FIXME: slice is slow on large set
        return (str(s)[:w] for s in self._accessor()[top:(top + h)])


class Grapheme(object):
    def __init__(self, type, data, x=None, y=None, depth=None):
        [setattr(self, k, v) for k, v in locals().items()]


# NEED: notion of 'focus', 'cursor', 'groups' => THINK decorated widgets (? how to composite ?)
# focus  :: dim unfocused widget, generate 'dying out' hi tail in list on cursor movement
# cursor :: 'bubble cursor' uses more then single line of output, pushing apart top and bottom pieces
# groups :: horiz delims between groups of files => special treatment, special fetcher to know where to place them
# baking :: compact path names to fit into widget size
class Widget(object):
    def __init__(self, view):
        self._view = view

    def __str__(self):
        ci = self._view.cursor._index
        return '\n'.join((str(v) if i != ci else str(v) + ' **')
                         for i, v in enumerate(self.data(None, None)))

    # DEV: must return abstract graphics stack tree
    #   ? THINK: Graphemes must be sorted by depth
    #     => draw as generator produces them
    #     -- impossible pre-sorting for 3D
    #     -- frontends may prefer different order of drawing
    def data(self, h, w):
        for i, e in enumerate(self._view.data(h, w)):
            if i == self._view.cursor._index:
                # vpos = cur_pos - top
                yield Grapheme('Cursor', e, x=0, y=i, depth=1)
            else:
                yield Grapheme('Line', e, x=0, y=i, depth=0)


# VIZ. horizontal, vertical, grid, etc
class Layout(object):
    def __init__(self, widget):
        self._widget = widget


# rename => Scene
# ALT:BAD: .draw() inside Widget()
#   BUT then you need links to all frontends inside widget itself
# WTF:(crash): list redraw when screen is shrinking
#   ? check screen height before each line draw ?
#   FIND: if ncurses supports 'virtual canvas' to draw on
#   FIND: how ncurses treats content when screen shrinks -- cuts it w/o restoring ?
# ALT:(faster) draw whole list -- then redraw line for cursor
#   => BUT: if we choose individual colors for entries -- no sense in optimization
class NcursesFrontend(object):
    def __init__(self, frmwk, widget):
        self._frmwk = frmwk
        self._widget = widget

    # ENH: supply layout as 'graphemes tree' to 'frontend conveyor' => REM ._widget
    def draw(self, state):
        stdscr, colorscheme = state['stdscr'], state['colorscheme']
        h, w = stdscr.getmaxyx()
        stdscr.clear()
        self.fill(stdscr, colorscheme, h, w)
        stdscr.refresh()

    def fill(self, stdscr, colorscheme, h, w):
        # DEV:CHG: derive from layout
        wx, wy = 1, 0
        wh, ww = h - wx, w - wy
        assert wh > 0 and ww > 0

        # THINK: use 'class' for g.type => isinstance()
        #   +++ inheritance for fallbacks -- replace specialized selection by regular one if not defined in colorscheme
        #   ++ compiler will chech imported class names for us
        #   -- can't create dispatch table
        graphemes = self._widget.data(wh, ww)
        for g in sorted(graphemes, key=lambda g: (g.depth, g.y)):
            if g.type == 'Line':
                stdscr.addstr(wy + g.y, wx + g.x, g.data, colorscheme[g.type])
            elif g.type == 'Cursor':
                stdscr.addstr(wy + g.y, wx + g.x, g.data, colorscheme[g.type])
                stdscr.move(wy + g.y, wx + g.x - 1)


# TEMP: hardcoded :: input => cursor
class NcursesInput(object):
    def __init__(self, frmwk, cursor):
        self._frmwk = frmwk
        self._cursor = cursor

    def dispatch(self, cmd, *args):
        _log.info('Cmd: {}'.format(cmd))
        f = getattr(self._cursor, cmd, '_err_wrong_cmd')
        return f(*args)

    def loop(self, stdscr, state):
        while True:
            key = stdscr.getkey()
            _log.info('Key: {}'.format(key))

            cmd = state['keymap'].get(key, None)
            if cmd is None:
                continue  # wrong key
            if cmd == 'quit':
                break  # TEMP:HACK:(asymmetrical) quit

            # TEMP:
            if cmd == 'redraw_all_now':
                send_event('redraw_all_now', state)

            state['input'].dispatch(cmd)
            send_event('redraw', state)


class NcursesFramework(object):
    def __init__(self, state):
        self._state = state

    def prepare(self):
        curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLACK)
        curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_CYAN)

        stdscr = self._state['stdscr']
        stdscr.attron(curses.color_pair(1))
        stdscr.nodelay(False)
        self._state['colorscheme'] = {
            'Line': curses.color_pair(1),
            'Cursor': curses.color_pair(2),
        }

    def loop(self, stdscr, loop):
        self._state['stdscr'] = stdscr
        self.prepare()
        send_event('redraw', self._state)
        loop(stdscr, self._state)

    def run(self, loop):
        curses.wrapper(self.loop, loop)
