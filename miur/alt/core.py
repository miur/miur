import uuid
import random
import curses
import logging
_log = logging.getLogger(__name__.split('.', 2)[1])

keymap = {
    'j': 'focus_node_next',
    'k': 'focus_node_prev',
    'h': 'shift_node_parent',
    'l': 'shift_node_current',
    '^L': 'redraw_all_now',
    'q': 'quit',
    '\n': 'quit',
}


def run(argv):
    dom = Dom()
    proxy = Proxy(dom)
    _log.info(proxy)
    cursor = Cursor(proxy, dom.uuid)
    view = View(cursor)
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


def flat_tree(dom, parent, n=0):
    edges = set(uuid.uuid4() for _ in range(random.randint(10, 20)))
    dom[parent] = edges | set([parent])
    if n > 0:
        for e in edges:
            flat_tree(dom, e, n - 1)


class Dom(object):
    def __init__(self):
        self.uuid = uuid.uuid4()
        self._data = {}
        flat_tree(self._data, self.uuid, 1)

    def __str__(self):
        return '\n'.join(str(i) for i in self.data())

    # TEMP:(assume list): get whole data
    def data(self):
        return self._data


# rename => View
# VIZ. DataProxy, ListProxy, MatrixProxy, ScalarProxy, DictProxy, TableProxy, RawProxy, ImageProxy
# ??? DEV Proxy per Node OR general one (NEED: ProxyNode anyways) ?
#   BET: general one => allows applying ops to whole tree (filtered)
class Proxy(object):
    def __init__(self, dom):
        self._dom = dom

    def __str__(self):
        return '\n'.join(str(i) for i in sorted(self.data()))

    # TEMP:(assume list):
    def data(self):
        return {pos: list(reversed(sorted(nodes)))[4:]
                for pos, nodes in self._dom.data().items()}


class Cursor(object):
    def __init__(self, proxy, init_node):
        self._proxy = proxy
        # TEMP:(cursor==index): hardcoded relation
        #   on delete file before cursor => decrease index
        # WTF:NEED: history cursor per each node
        self._index = 3  # EXPL:(None): only on empty list

        # WARN.path is history-like
        #   * random jump will be pushed despite being nonadjacent
        #   => moving back returns "back" and not "one-level-up"
        self.node = None
        # self.current = None
        self.path = []
        self.path_set = set()
        self.push(init_node)

    # NOTE: nodes in path may become invalid
    def push(self, node):
        if self.node is not None:
            self.path.append(self.node)
            self.path_set.add(self.node)
        self.node = node

    def pop(self):
        if not self.path:
            return
        old = self.node
        self.node = self.path.pop()
        self.path_set.remove(self.node)
        return old

    @property
    def edges(self):
        return self._proxy.data().get(self.node)

    @property
    def current(self):
        _log.debug('Current: {}'.format(self._index))
        return self.edges[self._index]

    def focus_node_next(self):
        if self._index is not None and self.edges is not None:
            self._index = min(self._index + 1, len(self.edges) - 1)

    def focus_node_prev(self):
        if self._index is not None and self.edges is not None:
            self._index = max(self._index - 1, 0)

    def shift_node_parent(self):
        self.pop()
        self._index = 0 if self.edges else None

    def shift_node_current(self):
        node = self.current
        if node in self.path_set:
            return  # cycle in-place
        if node not in self._proxy.data():
            return  # invalid node / leaf
        self.push(node)
        self._index = 0 if self.edges else None


# rename => Slice / Projection
# NOTE: combines Cursor, Size and Proxy to get Slice
# * acquires certain data type from DOM around cursor
#   +/- entries of list
#   elements of before/after path
# DEV: sep func 'crop list by area'
# DEV: sep func 'draw list in area'
#   ? how to do it bias-independent (~glPushMatrix()~)
#   TRY: impl by curses.newwin(height, width, begin_y, begin_x)
# HACK: generated on demand
#   * DOM may be flat list of undirected edges (pairs)
#   * View will bake() it to list of nodes centered around cursor
class View(object):
    def __init__(self, cursor):
        self.cursor = cursor

    # NOTE: evaluate scroll window indexes
    def top(self, lnum, h):
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
        items = self.cursor.edges
        top = self.top(len(items), h)
        # FIXME: slice is slow on large set
        return {'edges': (s for s in items[top:(top + h)])}


class Grapheme(object):
    def __init__(self, type, data, x=None, y=None, depth=None):
        [setattr(self, k, v) for k, v in locals().items()]


# THINK: who must convert objects to strings ?
#   * view => crop on width BAD: converts to string preliminary
#   * widget => does what it wants BAD: hardcoded to input obj classes
#       HACK: "View" may be reused to select all items on screen / visible in widget
class Widget(object):
    def __init__(self, view):
        self._view = view

    # DEV: must return abstract graphics stack tree (in terms of abstract frontend)
    #   ? THINK: Graphemes must be sorted by depth
    #     => draw as generator produces them
    #     -- impossible pre-sorting for 3D
    #     -- frontends may prefer different order of drawing
    def data(self, h, w):
        data = self._view.data(h, w)
        edges = list(data['edges'])
        curs = self._view.cursor.current
        yield Grapheme(
            'Line',
            '{:d} {:d}'.format(len(self._view.cursor.path), len(edges)),
            x=0, y=0, depth=0)
        yield Grapheme('Cursor', str(curs)[:w-5], x=5, y=0, depth=0)
        for i, e in enumerate(edges, 2):
            text = str(e)[:w]
            if e == curs:
                # vpos = cur_pos - top
                yield Grapheme('Cursor', text, x=0, y=i, depth=1)
            else:
                yield Grapheme('Line', text, x=0, y=i, depth=0)


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
        #       => use collections.ChainMap
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
