import os
import uuid
import random
import curses
import logging
import itertools
import datetime
from collections import Iterable
from memory_profiler import profile

from . import trace, proto

_log = logging.getLogger(__name__.split('.', 2)[1])
_memprof = open('/tmp/miur-memory.log', 'w+')

# TODO:(Lisp-style): supply cmd with args as tuple ('name', arg1, ...)
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


@profile(stream=_memprof)
def run(argv):
    dom = Dom(name='root')
    fs = TestGraphProvider()()
    dom.insert(fs, conn=(dom.roots(), fs.roots()))
    dom.update_node(cmd='ls -l', parents=dom.roots())
    dom.update_node(name='inc', provider=IncrementalProvider('/'), parents=dom.roots())

    proxy = DomProxy(dom)
    cursor = Cursor(proxy, dom.roots())
    proj = Projection(cursor)
    widget = Widget(proxy, proj)
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
    edges = Edges(uuid.uuid4() for _ in range(random.randint(10, 20)))
    dom[parent] = edges
    dom[parent].add(parent)
    if n > 0:
        for e in edges:
            flat_tree(dom, e, n - 1)


# ATT: can't inherit "Provider" from "Dom"
#   << because "Dom" is joined _cache_ of immediate results from "Providers"
class TestGraphProvider(object):
    def __call__(self):
        dom = Dom(name='test')
        flat_tree(dom._edges, dom.roots(), 2)
        # _log.info(dom)
        return dom


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
        m = g.roots()
        g._edges = {m: edges}
        g._types = {m: 'edges'}
        g._names = lines
        # datetime.datetime.utcfromtimestamp(timestamp).strftime("%A, %d. %B %Y %I:%M%p")
        # ALT: time.time() == datetime.datetime.utcnow().timestamp()
        g._names[m] = 'edges ' + str(datetime.datetime.now())
        return g


class IncrementalProvider(object):
    def __init__(self, path):
        self.path = path
        # _log.info(path)

    def __str__(self):
        return 'P[{}]'.format(self.path)

    def __call__(self):
        pwd, dirs, files = next(os.walk(self.path), (None, [], []))
        dirs = {uuid.uuid4(): e for e in dirs}
        files = {uuid.uuid4(): e for e in files}
        g = Dom()
        m = g.roots()
        g._names = dict(itertools.chain(dirs.items(), files.items()))
        g._edges = {m: Edges(g._names.keys())}
        g._types = {m: 'edges'}
        # HACK: insert dirs as lazy nodes => to trigger 'regenerate' in dom on access
        #   ATT:TEMP: use 'Edges()' instead of 'None' for .connect() to work correctly
        g._edges.update({k: Edges() for k in dirs})
        g._providers = {k: IncrementalProvider(pwd + '/' + v)
                        for k, v in g._names.items()}
        # _log.debug('\n'.join('{!s}: {!s}'.format(k, v) for k, v in g._providers.items()))

        g._names[m] = 'exec dir ' + str(datetime.datetime.now())
        # g._providers[m] = self
        return g


# NOTE: reduced api to abstract the container
class Edges(object):
    def __init__(self, iterator=None):
        self._container = set(iterator) if iterator else set()

    def __contains__(self, item):
        return item in self._container

    def __len__(self):
        return len(self._container)

    def __iter__(self):
        return iter(self._container)

    # SEE https://stackoverflow.com/questions/5288990/in-python-what-operator-to-override-for-if-object
    def __bool__(self):
        return bool(self._container)

    def add(self, item):
        self._container.add(item)

    def remove(self, item):
        self._container.remove(item)

    def update(self, iterator):
        self._container.update(iterator)


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
    def __init__(self, name=None):
        # Wrap uuid into simple "Node" class to hide impl
        #   ALSO: default ctor gens new uuid ALT=(from uuid import uuid4 as Node)
        #   NOT: {uid != Node} => because Node contains all its attributes (or accessors to attributes)
        #     ALT: treat {Node == uid} for abstract graph and use some other obj
        #     name for all node attrs -- like "Desc/Cell/Object/NodeProxy/Avatar"
        #     * embed accessors attr(dom, uid) as properties OR preeval values
        # BUG: can't name ._uid because of merging doms in .insert()
        self.uid = uuid.uuid4()  # ENH:TEMP: using .uid means all *dom* are tree
        # FIXME:ADD:(self._uids): leaf nodes don't have edges at all
        #   << BUT I need single place of truth for 'uid in dom' and gen new 'uid'
        self._edges = {}
        # HACK: add itself => because "Dom" is "Node" itself
        self._edges[self.roots()] = Edges()
        self._types = {}
        self._providers = {}
        # BAD: nodes may have different names depending on location
        #   E.G. instead of hiding -- use name '..' for parent node in each dir
        #   ?? add dicts per node for local names -- as they are viewed ??
        #     << each node may have its own conception/names about all other nodes
        #     IMPL: self._alias = {uid: {uid: alias, ...}}
        #     ALT:ALG: name = cursor==node ? '..' : dom.nameof(node)
        self._names = {}
        if name is not None:
            self._names[self.roots()] = name

        # TEMP: insert individual transformations
        #   BET:RFC: allow shared/inherited transf between multiple w/o explicit assign
        # transf = Transformation()
        # self._transfs = {e: transf for e in self}
        self._transfs = {}

    # TEMP: return list of special points of graph
    #   :: tree => root, cyclic_graph => node[0], empty => None
    def roots(self):
        return self.uid

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
                _log.debug('{} <=> {}'.format(f, t))
                self._edges[f].add(t)
                self._edges[t].add(f)

    def add_node(self, *, node=None, edges=None):
        if node is None:
            node = uuid.uuid4()  # CHG= Node()
        # always add at least empty virt node
        self._edges[node] = edges or Edges()  # TEMP:FIXED: empty multichoice per node
        return node

    def conn_node(self, uid, parents):
        assert uid
        # assert parents
        # if parents is None:
        #     return
        # if not isinstance(parents, collections.Iterable):
        #     parents = [parents]
        for p in parents:
            self._edges[p].add(uid)  # silently ignore adding same uid

    # NOTE: this func is actually provider and *dom* is immediate cache of everything
    #   => on demand -- provide already cached value or query/rebuild it (and store to history)
    def update_node(self, *, node=None, edges=None, name=None, cmd=None, provider=None, parents=None):
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
        elif provider is not None:
            self._providers[node] = provider
        # connect to root node ATT: last cmd
        self.conn_node(node, parents)
        return node

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
    # def metainfo(self, meta):
    #     if meta in ['edges', 'names', 'transfs']:
    #         return getattr(self, '_' + meta, None)

    def regenerate(self, uid):
        g = self._providers[uid]()
        self.insert(g, conn=(uid, g.roots()))
        # _log.info(g)

    # TRY: depending on kw 'real=True' choose from several impl dicts
    #   THINK: copy edges for read-only access
    # WARN: keep this method => regeneration must be inside Dom NOT Proxy
    #   DEV: combine methods to attrof(uid, attr)
    def edgesof(self, uid, dfl=None):
        if dfl is not None and uid not in self:
            return dfl
        # TEMP: evaluate shellcommand each time
        #   => BUG: cursor looses position, keeping pointing to nonexistent edge
        edges = self._edges[uid]
        # _log.critical('{} ::: {}'.format(uid, len(edges)))
        # FIXME: reexec cmd only on '<Enter>' => MOVE sep function
        # CHG currently caches only if multichoice empty NEED exec if never exec
        if uid in self._providers:
            try:
                # NOTE: Edges() may have virtual nodes even befor real were generated
                self.node_byattr(uid, 'edges')
            except KeyError:
                self.regenerate(uid)
                # UNUSED:HACK: works w/o reassign if edges != None
                # edges = self._edges[uid]
        return edges

    # TEMP:CHG: returns single possible view per node (BAD)
    def viewof(self, uid):
        try:
            view = self.node_byattr(uid, 'view')
        except KeyError:
            edges = self.edgesof(uid)
            transf = Transformation(self)  # self._transfs.get(uid)
            view_uid = uuid.uuid4()
            # NOTE: 'view' is ordered list
            view = list(transf(edges) if transf else edges)
            self._types[view_uid] = 'view'
            self._edges[view_uid] = view
            self._edges[view_uid].append(uid)
            self._edges[uid].add(view_uid)
        return view

    def nameof(self, uid):
        for ns in [self._names]:
            if uid in ns:
                return str(ns[uid])
        return str(uid)

    def node_byattr(self, uid, attrtype):
        # TEMP: match very first uid with type
        for e in self._edges[uid]:
            if self._types.get(e) == attrtype:
                return e
        raise KeyError  # TEMP: if not found -- work as hasattr()


# NOTE: all inc ops add funcs to transf chain
class Transformation(object):
    def __init__(self, dom):
        # BAD: for sorting by name you need access to Dom.nameof() ALSO slow
        sort = (lambda a: sorted(a, key=lambda x: dom.nameof(x)))  # TEMP:RFC
        # sort = (lambda a: sorted(a, key=lambda x: x.name))  # ALT for NodeProxy
        self.chain = [sort]  # , reversed, list, lambda o: o[4:]

    def __call__(self, obj):
        for f in self.chain:
            obj = f(obj)
        return obj


# NOTE: abstract cached lazy accessor to node attrs
#   => _only_ for gathering all attrs together in horiz/vert model
#   !! NOT for sorting/trimming
# ALT:DEV: direct access to underlying data by Dom.attrof(self, node, attr)
# USE: invalidate() on event "node in dom has changed"
class NodeProxy(object):
    def __init__(self, dom, uid):
        self._dom = dom
        self._uid = uid

    def invalidate(self):
        del self.edges  # mark edges to be reevaled on next access

    @proto.cached_property
    def name(self):
        return self._dom.nameof(self._uid)

    @proto.cached_property
    def edges(self):
        _log.debug('gen edges')
        return self._dom.edgesof(self._uid)

    # TEMP:CHG: caches single possible view per node (BAD)
    #   NEED: supply external Transformation and store results it as new node
    #   with ref in types to this supplied Transformation to gen peculiar view
    @proto.cached_property
    def view(self):
        return self._dom.viewof(self._uid)


# RFC: .edges required very often => cache results directly inside proxy
#   << otherwise chain of nested dom.edgesof() requires too much func calls
# rename => View / DomAccessor
# VIZ. DataProxy, ListProxy, MatrixProxy, ScalarProxy, DictProxy, TableProxy, RawProxy, ImageProxy
# ??? DEV Proxy per Node OR general one (NEED: ProxyNode anyways) ?
#   BET: general one => allows applying ops to whole tree (filtered)
# TEMP: embed caching into proxy
# NEED: inherit same interface as "Dom" for transparent access
# BAD: too much call levels => BAD: meaningless slow data-driven development arch
#   E.G. nesting / overriding __iter__
class DomProxy(object):
    def __init__(self, dom):
        self._dom = dom
        self._nodescache = {}  # TEMP:ENH: generalize caching BAD: never freed

    def __str__(self):
        return '\n'.join(str(i) for i in sorted(self.data()))

    def __contains__(self, uid):
        # if isinstance(uid, NodeProxy):
        #     uid = uid.uid
        return uid in self._dom

    def __getitem__(self, uid):
        if uid not in self._nodescache:
            self._nodescache[uid] = NodeProxy(self._dom, uid)
        return self._nodescache[uid]

    # USAGE: list(dom.asnodes(dom[uid].edges))
    # OR: return list(transf(NodeProxy(self._dom, e) for e in self._dom.edgesof(self._uid)))
    # def asnodes(self, uids):
    #     for uid in uids:
    #         yield NodeProxy(self._dom, uid)

    # TEMP:REM: intermediate accessor
    def regenerate(self, uid):
        return self._dom.regenerate(uid)


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
        self.cur_node = None
        # self.cur_edge = None
        self.path = []
        self.path_set = set()
        self.pos_path = []      # store parent positions for backward (loops in path)
        self.pos_visited = {}   # store last positions for forward
        self.move_forward(init_node)
        self.view_visited = {}  # points to view used by this Cursor

    def move_forward(self, uid):
        if self.cur_node is not None:
            # NOTE: don't store nodes directly in path
            #   => nodes must be allowed to disappear on delete
            self.path.append(self.cur_node)
            self.path_set.add(self.cur_node)
        self.cur_node = uid
        self.pos_path.append(self._index)
        # EXPL:(None): if empty list in next node
        self._index = self.pos_visited.get(uid, 0) if self.edges else None

    # FIXME: old nodes in path may become invalid => skip until valid ones
    def hist_back(self):
        old = self.cur_node
        self.cur_node = self.path.pop()
        self.path_set.remove(self.cur_node)
        self.pos_visited[old] = self._index
        # EXPL:(None): if prev node deleted or its list emptied
        self._index = self.pos_path.pop() if self.edges else None
        return old

    @property
    def edges(self):
        edges = self._dom[self.cur_node].view
        if not edges or not self.path:
            return edges
        # NOTE: additional Transformation for Cursor
        # BAD: slow filtering on each access
        #   BUT: can't cache as view in *dom*
        #     => if multiple cursor walked here from multiple links
        return [e for e in edges if e != self.path[-1]]

    @property
    def cur_edge(self):
        if self._index is not None:
            # BUG: exception on empty dir << _index=None
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
        uid = self.cur_edge
        if uid in self.path_set:
            return  # cycle in-place
        if uid not in self._dom:
            _log.debug('Leaf[END] :: {}'.format(uid))
            return  # invalid uid / leaf
        self.move_forward(uid)

    def regenerate_now(self):
        return self._dom.regenerate(self.cur_edge)


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
#   * Projection will bake() it to list of nodes centered around cursor
class Projection(object):
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
    def __init__(self, dom, proj):
        self._dom = dom
        self._proj = proj

    # DEV: must return abstract graphics stack tree (in terms of abstract frontend)
    #   ? THINK: Graphemes must be sorted by depth
    #     => draw as generator produces them
    #     -- impossible pre-sorting for 3D
    #     -- frontends may prefer different order of drawing
    def graphemes(self, h, w):
        x, y = 0, 2
        data = self._proj.data(h-y, w-x)
        edges = list(data['edges'])
        cedge = self._proj.cursor.cur_edge
        idx = 1 + (self._proj.cursor._index or -1)
        status = '{:d}: {:2d}/{:02d} | {} | {:d}kiB'.format(
            1 + len(self._proj.cursor.path), idx,
            len(edges), self._proj.cursor.cur_node, proto.meminfo()//1024)
        yield Grapheme('Cursor', status, x=0, y=0, depth=0)
        for i, e in enumerate(edges):
            text = self._dom[e].name
            text = text[:w-x]
            if e == cedge:
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
        # _log.debug([x, y, h, w, n])
        ww = (w // n) if self._direc == 0 else w
        wh = (h // n) if self._direc == 1 else h
        wx = (w * i // n) if self._direc == 0 else 0
        wy = (h * i // n) if self._direc == 1 else 0
        # _log.debug([wx, wy, wh, ww])
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
# IDEA: bake scene in graph and add to root nodes to actually view in *miur*
#   => format for intermediate lang is the same as *dom*
#   http://baikalweb.jinr.ru/doc/cern_doc/asdoc/gks_html3/node6.html
#   CHECK: you can't view scene graph realtime => recursion
#   BUT: all node attributes must be encapsulated in single grapheme ?
#       => convert horiz/vert *dom* impl
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
            _log.debug('current :: {}'.format(self._cursor.cur_edge))
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
