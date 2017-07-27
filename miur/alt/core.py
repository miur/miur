import uuid
import random
import curses
import logging
import itertools
import datetime
from collections import Iterable

from . import trace, proto

_log = logging.getLogger(__name__.split('.', 2)[1])

keymap = {
    'j': 'focus_node_next',
    'k': 'focus_node_prev',
    'g': 'focus_node_first',
    'M': 'focus_node_middle',
    'G': 'focus_node_last',
    'h': 'shift_node_parent',
    'l': 'shift_node_current',
    '1': lambda: trace.setloglevel(trace.TRACE),
    '2': lambda: trace.setloglevel(logging.DEBUG),
    '3': lambda: trace.setloglevel(logging.INFO),
    '^L': 'redraw_all_now',
    '\n': ['regenerate_now', 'shift_node_current'],
    'q': 'quit',
    '\033': 'quit',
}


def run(argv):
    dom = Dom()
    flat_tree(dom._edges, dom.uid, 2)
    dom.update_node(cmd='ls -l', parents=dom.uid)

    # proxy = Proxy(dom)
    # _log.info(proxy)
    cursor = Cursor(dom, dom.uid)
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


# NOTE: individual per each multichoice aspect to regenerate it
# * ShellProvider == shellexec + cmdline
# * NativeProvider == os.listdirs()
# THINK:(suggestion): if provider == None => cache is unique and non-regenerable
# DEV: must generate detached *dom* => for structured cmds like 'ag'
#   NEED: special parsers per each shell cmd output type
class ShellProvider(object):
    def __init__(self, cmd):
        self.cmd = cmd

    # NOTE return list of nodes to include in *dom*
    #   * generated from expression
    def __call__(self):
        # TEMP:(hardcoded): linewise
        edges, lines = proto.cmd2dom(self.cmd)
        # WTF: how to generate Dom effectively ?
        g = Dom()
        g._edges = {g.uid: edges}
        g._names = lines
        # datetime.datetime.utcfromtimestamp(timestamp).strftime("%A, %d. %B %Y %I:%M%p")
        # ALT: time.time() == datetime.datetime.utcnow().timestamp()
        g._names[g.uid] = datetime.datetime.now()
        return g


# TODO: encapsulate each dict of "Dom" into vector "Attribute"
#   => then keep them in C++ but drop in python (as all objects are dicts anyways)
class Attribute(object):
    pass


# NOTE: even Dom must encapsulate all access to elements
#   => so it could sync with fs immediately on demand in blocking ops
# BAD:(flat model): force generators caching => repeats
#   * force caching == traverse all nodes and replace edges=None by gen lists
#   * BUT: after replacing it will be inserted back into original *dom*
#   * BAD: recursive process => each time walks through already generated nodes
class Dom(object):
    def __init__(self):
        # Wrap uuid into simple "Node" class to hide impl
        #   ALSO: default ctor gens new uuid ALT=(from uuid import uuid4 as Node)
        #   NOT: {uid != Node} => because Node contains all its attributes (or accessors to attributes)
        #     ALT: treat {Node == uid} for abstract graph and use some other obj
        #     name for all node attrs -- like "Desc/Cell/Object/NodeProxy/Avatar"
        #     * embed accessors attr(dom, uid) as properties OR preeval values
        self.uid = uuid.uuid4()  # ENH:TEMP: using .uid means all *dom* are tree
        self._edges = {}
        self._providers = {}
        # BAD: nodes may have different names depending on location
        #   E.G. instead of hiding -- use name '..' for parent node in each dir
        #   ?? add dicts per node for local names -- as they are viewed ??
        #     << each node may have its own conception/names about all other nodes
        #     IMPL: self._alias = {uid: {uid: alias, ...}}
        self._names = {}

        # TEMP: insert individual transformations
        #   BET:RFC: allow shared/inherited transf between multiple w/o explicit assign
        # transf = Transformation()
        # self._transfs = {e: transf for e in self}
        self._transfs = {}

    # NOTE: allowed unlinked graphs => cursor directly jumps to uid
    #   BAD: listing all separate unlinked graphs is impossibly slow operation
    #     (need to traverse whole graph and trace all edges)
    #   ~~ cursor must somehow know uids of those separate graphs
    def insert(self, g, conn):
        assert isinstance(g, self.__class__)
        for attr in vars(g).keys():
            if not attr.startswith('_') or attr.startswith('__'):
                continue
            vals = getattr(g, attr)
            # if not callable(vals) and isinstance(vals, dict):
            getattr(self, attr).update(vals)
        if conn is not None:
            self.connect(conn)

    def connect(self, conn):
        assert conn
        if isinstance(conn, tuple):
            conn = [conn]
        for fs, ts in conn:
            if not isinstance(fs, Iterable):
                fs = [fs]
            if not isinstance(ts, Iterable):
                ts = [ts]
            for f, t in itertools.product(fs, ts):
                self._edges[f].add(t)
                self._edges[t].add(f)

    def add_node(self, *, node=None, edges=None):
        if node is None:
            node = uuid.uuid4()  # CHG= Node()
        # always add at least empty virt node
        self._edges[node] = edges or set()  # TEMP:FIXED: empty multichoice per node
        return node

    def conn_node(self, node, parents):
        assert node
        # assert parents
        # if parents is None:
        #     return
        # if not isinstance(parents, collections.Iterable):
        #     parents = [parents]
        for p in parents:
            self._edges[p].add(node)  # silently ignore adding same node

    # NOTE: this func is actually provider and *dom* is immediate cache of everything
    #   => on demand -- provide already cached value or query/rebuild it (and store to history)
    def update_node(self, *, node=None, edges=None, name=None, cmd=None, parents=None):
        if parents is not None and not isinstance(parents, Iterable):
            parents = [parents]
        node = self.add_node(node=node, edges=edges)
        # TEMP:FIXME?XXX insert parent to edges
        if edges is not None and parents:
            self._edges[node].update(parents)

        # name for virtual node itself
        if name is None and cmd is not None:
            name = cmd
        if name is not None:
            self._names[node] = name
        if cmd is not None:
            self._providers[node] = ShellProvider(cmd)
        # connect to root node ATT: last cmd
        self.conn_node(node, parents)
        return node

    def _shexec(self, node):
        # TEMP: only names metainfo subsystem
        # TEMP: combine nodes with cmd results
        # TODO: self._cmds => to execute cmd on-demand when opening cmd
        #   => then impl cmd caching to re-execute it only on <Enter> and use cache by default
        #   NEED: single point api to access edges of node
        edges, lines = proto.cmd2dom(self._shellcommands[node], parent=self.uid)
        self._edges[node] = edges           # cache cmd results
        self._edges[node].add(self.uid)     # insert parent to edges
        self._names.update(lines)  # XXX how to combine with add_node() ?
        return edges

    def __len__(self):
        return len(self._edges)

    def __iter__(self):
        for uid in self._edges:
            yield uid

    def __contains__(self, uid):
        return uid in self._edges

    # DEV: generate complete node with fieds pointing to each metainfo subsystem
    # def __getitem__(self, uid):
    #     return self._edges[uid]

    def __str__(self):
        return '\n'.join(str(uid) for uid in self)

    # SECU: only allowed attrs
    def metainfo(self, meta):
        if meta in ['edges', 'names', 'transfs']:
            return getattr(self, '_' + meta, None)

    def regenerate(self, node):
        g = self._providers[node]()
        self.insert(g, conn=(node, g.uid))
        _log.info(g)

    # TRY: depending on kw 'real=True' choose from several impl dicts
    #   THINK: copy edges for read-only access
    def edgesof(self, node, dfl=None):
        if dfl is not None and node not in self:
            return dfl
        # TEMP: evaluate shellcommand each time
        #   => BUG: cursor looses position, keeping pointing to nonexistent edge
        edges = self._edges[node]
        # FIXME: reexec cmd only on '<Enter>' => MOVE sep function
        # CHG currently caches only if multichoice empty NEED exec if never exec
        if not edges and node in self._providers:
            self.regenerate(node)
        if edges:
            transf = self._transfs.get(node)
            return edges if transf is None else transf(edges)

    def nameof(self, node):
        for ns in [self._names]:
            if node in ns:
                return str(ns[node])
        return str(node)


# NOTE: all inc ops add funcs to transf chain
class Transformation(object):
    def __init__(self):
        # BAD: for sorting by name you need access to Dom.nameof() ALSO slow
        self.chain = [sorted, reversed, list]  # , lambda o: o[4:]

    def __call__(self, obj):
        for f in self.chain:
            obj = f(obj)
        return obj


# UNUSED:
# rename => View / DomAccessor
# VIZ. DataProxy, ListProxy, MatrixProxy, ScalarProxy, DictProxy, TableProxy, RawProxy, ImageProxy
# ??? DEV Proxy per Node OR general one (NEED: ProxyNode anyways) ?
#   BET: general one => allows applying ops to whole tree (filtered)
# TEMP: embed caching into proxy
# NEED: inherit same interface as "Dom" for transparent access
# BAD: too much call levels => BAD: meaningless slow data-driven development arch
#   E.G. nesting / overriding __iter__
class Proxy(object):
    def __init__(self, dom):
        self._dom = dom
        self.transf = Transformation()

    def __str__(self):
        return '\n'.join(str(i) for i in sorted(self.data()))

    # TEMP:(assume list):
    def data(self):
        return {p: self.transf(edges) for p, edges in self._dom.data().items()}


# NOTE: containedin cursor to tweak traversing strategy
#   ALSO: deterministic jump over multichoice
# NOTE: no need to chg cursor arch for multichoice
#   * has strategy to choose choice => store jump target in .path
#   * moving back => to place from where jump was done
#   * if no hiding => all OK => return back to multichoice
class Strategy(object):
    pass


# IDEA: filter-out (un)visited nodes in current dir
#   ALT: generate virtual view instead of filtering proxy
#   * focus only on several dirs picked by hands
#   * or hide dir after it was visited once
class Cursor(object):
    def __init__(self, dom, init_node):
        self._dom = dom
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
        edges = list(self._dom.edgesof(self.node))
        if not edges or not self.path:
            return edges
        return [e for e in edges if e != self.path[-1]]

    @property
    def current(self):
        return self.edges[self._index]

    def focus_node_next(self):
        if self._index is not None and self.edges is not None:
            self._index = min(self._index + 1, len(self.edges) - 1)

    def focus_node_prev(self):
        if self._index is not None and self.edges is not None:
            self._index = max(self._index - 1, 0)

    def focus_node_first(self):
        if self._index is not None and self.edges is not None:
            self._index = 0

    def focus_node_middle(self):
        if self._index is not None and self.edges is not None:
            self._index = (len(self.edges) - 1) // 2

    def focus_node_last(self):
        if self._index is not None and self.edges is not None:
            self._index = len(self.edges) - 1

    def shift_node_parent(self):
        if not self.path:
            return
        self.hist_back()

    def shift_node_current(self):
        node = self.current
        if node in self.path_set:
            return  # cycle in-place
        if node not in self._dom:
            return  # invalid node / leaf
        self.move_forward(node)

    def regenerate_now(self):
        return self._dom.regenerate(self.current)


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
        status = '{:d}: {:2d}/{:02d} | {} | {:d}kiB'.format(
            1 + len(self._view.cursor.path),
            1 + self._view.cursor._index,
            len(edges), self._view.cursor.node, proto.meminfo()//1024)
        yield Grapheme('Cursor', status, x=0, y=0, depth=0)
        for i, e in enumerate(edges):
            text = self._dom.nameof(e)
            text = text[:w-x]
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


# NOTE: encapsulates nested layouts composition (scaled)
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
# TODO: primitives to renderer must be supplied in independent format
#   => so I could re-implement renderer in any lang and load *.so on demand
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

    def dispatch(self, cmd):
        if not isinstance(cmd, list):
            cmd = [cmd]
        # TEMP: chain => use last result
        #   ALT: pipe => pass results to next command
        #   SEE: haskell monads for these cases
        for c in cmd:
            if callable(c):
                f = c
            elif isinstance(c, str):
                f = getattr(self._cursor, c)
            r = f()
        return r

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
