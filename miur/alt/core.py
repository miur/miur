import uuid
import random
import curses
import logging

from . import trace, proto

_log = logging.getLogger(__name__.split('.', 2)[1])

keymap = {
    'j': 'focus_node_next',
    'k': 'focus_node_prev',
    'h': 'shift_node_parent',
    'l': 'shift_node_current',
    '1': lambda: trace.setloglevel(trace.TRACE),
    '2': lambda: trace.setloglevel(logging.DEBUG),
    '3': lambda: trace.setloglevel(logging.INFO),
    '^L': 'redraw_all_now',
    'q': 'quit',
    '\n': 'quit',
}


def run(argv):
    dom = Dom()
    proxy = Proxy(dom)
    # _log.info(proxy)
    cursor = Cursor(proxy, dom.uuid)
    view = View(cursor)
    widget = Widget(dom, view)
    layout = BoxLayout([widget])
    scene = Scene(layout, None)

    state = {'keymap': keymap, 'scene': scene}
    nc_frmwk = NcursesFramework(state)
    nc_rendr = NcursesRenderer(nc_frmwk, state)
    nc_input = NcursesInput(nc_frmwk, cursor)
    state['render'] = nc_rendr
    state['input'] = nc_input
    nc_frmwk.run(nc_input.loop)


def send_event(event, state):
    if event == 'redraw':
        # DEV: skip some draw requests to limit fps
        rendr = state['render']
        scene = state['scene'].bake(*rendr.size)
        rendr.draw(scene, state)


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
        flat_tree(self._data, self.uuid, 2)

        # TEMP: only names metainfo subsystem
        # TEMP: combine nodes with cmd results
        # TODO: self._cmds => to execute cmd on-demand when opening cmd
        #   => then impl cmd caching to re-execute it only on <Enter> and use cache by default
        #   NEED: single point api to access edges of node
        self._names = {}
        cmd = 'ls'
        node, edges, metainfo = proto.cmd2dom('ls', parent=self.uuid)
        self._data[node] = edges
        self._data[self.uuid].add(node)
        self._names[node] = cmd
        self._names.update(metainfo)

    def __str__(self):
        return '\n'.join(str(i) for i in self.data())

    # TEMP:(assume list): get whole data
    def data(self):
        return self._data


class Transformation(object):
    def __init__(self):
        self.chain = [sorted, reversed, list]  # , lambda o: o[4:]

    def __call__(self, obj):
        for c in self.chain:
            obj = c(obj)
        return obj


# rename => View
# VIZ. DataProxy, ListProxy, MatrixProxy, ScalarProxy, DictProxy, TableProxy, RawProxy, ImageProxy
# ??? DEV Proxy per Node OR general one (NEED: ProxyNode anyways) ?
#   BET: general one => allows applying ops to whole tree (filtered)
class Proxy(object):
    def __init__(self, dom):
        self._dom = dom
        self.transf = Transformation()

    def __str__(self):
        return '\n'.join(str(i) for i in sorted(self.data()))

    # TEMP:(assume list):
    def data(self):
        return {p: self.transf(edges) for p, edges in self._dom.data().items()}


# IDEA: filter-out (un)visited nodes in current dir
#   ALT: generate virtual view instead of filtering proxy
#   * focus only on several dirs picked by hands
#   * or hide dir after it was visited once
class Cursor(object):
    def __init__(self, proxy, init_node):
        self._proxy = proxy
        # TEMP:(cursor==index): hardcoded relation
        #   on delete => search valid file before cursor => decrease index in cycle
        self._index = None

        # WARN.path is history-like
        #   * random jump will be pushed despite being nonadjacent
        #   => moving back returns "back" and not "one-level-up"
        self.node = None
        # self.current = None
        self.path = []
        self.path_set = set()
        self.pos_path = []      # store parent positions for backward (loops in path)
        self.pos_visited = {}   # store last positions for forward
        self.move_forward(init_node)

    def move_forward(self, node):
        if self.node is not None:
            self.path.append(self.node)
            self.path_set.add(self.node)
        self.node = node
        self.pos_path.append(self._index)
        # EXPL:(None): if empty list in next node
        self._index = self.pos_visited.get(node, 0) if self.edges else None

    # FIXME: old nodes in path may become invalid => skip until valid ones
    def hist_back(self):
        old = self.node
        self.node = self.path.pop()
        self.path_set.remove(self.node)
        self.pos_visited[old] = self._index
        # EXPL:(None): if prev node deleted or its list emptied
        self._index = self.pos_path.pop() if self.edges else None
        return old

    @property
    def edges(self):
        return [e for e in self._proxy.data().get(self.node) if e != self.node]

    @property
    def current(self):
        return self.edges[self._index]

    def focus_node_next(self):
        if self._index is not None and self.edges is not None:
            self._index = min(self._index + 1, len(self.edges) - 1)

    def focus_node_prev(self):
        if self._index is not None and self.edges is not None:
            self._index = max(self._index - 1, 0)

    def shift_node_parent(self):
        if not self.path:
            return
        self.hist_back()

    def shift_node_current(self):
        node = self.current
        if node in self.path_set:
            return  # cycle in-place
        if node not in self._proxy.data():
            return  # invalid node / leaf
        self.move_forward(node)


# rename => Slice / Projection / Camera
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


# rename => Primitive
# BAD: namedtuple unmutable => can't add offset in layout
# Grapheme = collections.namedtuple('Grapheme', 'type data x y depth')
class Grapheme(object):
    def __init__(self, type, data, x=None, y=None, depth=None):
        [setattr(self, k, v) for k, v in locals().items()]


class Widget(object):
    def __init__(self, dom, view):
        self._dom = dom
        self._view = view

    # DEV: must return abstract graphics stack tree (in terms of abstract frontend)
    #   ? THINK: Graphemes must be sorted by depth
    #     => draw as generator produces them
    #     -- impossible pre-sorting for 3D
    #     -- frontends may prefer different order of drawing
    def graphemes(self, h, w):
        x, y = 0, 2
        data = self._view.data(h-x, w-y)
        edges = list(data['edges'])
        curs = self._view.cursor.current
        status = '{:d}: {:2d}/{:02d} | {}'.format(
            1 + len(self._view.cursor.path),
            1 + self._view.cursor._index,
            len(edges), self._view.cursor.node)
        yield Grapheme('Cursor', status, x=0, y=0, depth=0)
        for i, e in enumerate(edges):
            text = self._dom._names.get(e, e)
            text = str(text)[:w-x]
            if e == curs:
                # vpos = cur_pos - top
                yield Grapheme('Cursor', text, x=x, y=y+i, depth=1)
            else:
                yield Grapheme('Line', text, x=x, y=y+i, depth=0)


# VIZ. horizontal, vertical, grid, etc
class BoxLayout(object):
    def __init__(self, widgets, direc=1):
        self._widgets = widgets
        self._direc = direc

    def rect_i(self, h, w, i):
        x, y = 2, 1  # margin / padding
        h, w = h-y, w-x
        n = len(self._widgets)
        _log.debug([x, y, h, w, n])
        ww = (w // n) if self._direc == 0 else w
        wh = (h // n) if self._direc == 1 else h
        wx = (w * i // n) if self._direc == 0 else 0
        wy = (h * i // n) if self._direc == 1 else 0
        _log.debug([wx, wy, wh, ww])
        assert wh > 0 and ww > 0
        return x + wx, y + wy, wh, ww

    def bake(self, h, w):
        for i, widget in enumerate(self._widgets):
            wx, wy, wh, ww = self.rect_i(h, w, i)
            for g in widget.graphemes(wh, ww):
                g.x += wx
                g.y += wy
                yield g


# NOTE: incapsulates nested layouts composition (scaled)
#   + flat list of all widgets
#   + focus cursor
#   + cache data
#   * multiple independent windows => TabbedLayout
#     => usable even for replacing single widget in-place by any other layout
class Scene(object):
    def __init__(self, layout, focus):
        self._layout = layout
        self._focus = focus

    def bake(self, h, w):
        # self._layout.resize(h, w)
        return self._layout.bake(h, w)


# rename => Scene / Frontend
# ALT:BAD: .draw() inside Widget()
#   BUT then you need links to all frontends inside widget itself
# WTF:(crash): list redraw when screen is shrinking
#   ? check screen height before each line draw ?
#   FIND: if ncurses supports 'virtual canvas' to draw on
#   FIND: how ncurses treats content when screen shrinks -- cuts it w/o restoring ?
# ALT:(faster) draw whole list -- then redraw line for cursor
#   => BUT: if we choose individual colors for entries -- no sense in optimization
# NOTE:TEMP: (scene == layout) completely fill window screen
#   ? how tmux fills up only part of screen ? => may be useful
class NcursesRenderer(object):
    def __init__(self, frmwk, state):
        self._frmwk = frmwk
        self._state = state

    @property
    def size(self):
        return self._state['stdscr'].getmaxyx()

    # ENH: supply layout as 'graphemes tree' to 'frontend conveyor' => REM ._widget
    def draw(self, graphemes, state):
        stdscr, colorscheme = state['stdscr'], state['colorscheme']
        stdscr.clear()
        self.render(stdscr, graphemes, colorscheme)
        stdscr.refresh()

    # THINK: use 'class' for g.type => isinstance()
    #   +++ inheritance for fallbacks -- replace specialized selection by regular one if not defined in colorscheme
    #   ++ compiler will check imported class names for us
    #   -- can't create dispatch table
    def render(self, stdscr, graphemes, colorscheme):
        for g in sorted(graphemes, key=lambda g: (g.depth, g.y, g.x)):
            try:
                if g.type == 'Line':
                    stdscr.addstr(g.y, g.x, g.data, colorscheme[g.type])
                elif g.type == 'Cursor':
                    stdscr.addstr(g.y, g.x, g.data, colorscheme[g.type])
                    stdscr.move(g.y, g.x - 1)
            except curses.error:
                _log.error('{}: small screen'.format(self.__class__.__name__))


# TEMP: hardcoded :: input => cursor
class NcursesInput(object):
    def __init__(self, frmwk, cursor):
        self._frmwk = frmwk
        self._cursor = cursor

    def dispatch(self, cmd, *args):
        if callable(cmd):
            f = cmd
        elif isinstance(cmd, str):
            f = getattr(self._cursor, cmd, '_err_wrong_cmd')
        return f(*args)

    def loop(self, stdscr, state):
        while True:
            key = stdscr.getkey()
            cmd = state['keymap'].get(key, None)
            _log.info('{} :: {}'.format(key, cmd))

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
        curses.init_pair(1, 7, 8)
        curses.init_pair(2, 8, 4)

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
