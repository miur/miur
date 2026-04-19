#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
from . import proto, graph


def run(argv):
    g = GraphProxy(graph.ObjectGraph())  # empty
    Transformation()(g)
    for n in g.get_root():
        print(n.get())


# ALT: self._signal.emit('on_get_val') for arbitrary set of signals
class NodeSignal(object):
    def __init__(self, node):
        self._node = node

    def on_get_val(self):
        print("***")

    def on_iter_each(self):
        print("*")

    def on_iter_beg(self):
        print(">")

    def on_iter_end(self):
        print("<")


# NOTE: must be inherited from slots
#   BUT: no need to dispatch signal-slot if strategy is inside the same node
#     = less overhead, no additional id used
class LazyStrategy(NodeSignal):
    def on_iter_beg(self):
        super().on_iter_beg()
        # FIXME: pass graph in Tr.__call__ -- because Tr is not binded to G
        #   => otherwise create BindedTransformation -- for concrete G
        #   => e.g. G.Tr_factory() which produces BindedTransformation
        # tr = Transformation(self._node.g, self._node)
        #   NOTE: => it uses provider from inside node to generate new nodes
        #   use specialized AppendTransformation() to insert new nodes
        # self._node.g.transform(tr)  # XXX???


# NOTE:
#  * pack each entity into proxy
#  * proxy triggers events on api
class NodeProxy(object):
    def __init__(self, g, uid):
        self._g = g
        self._uid = uid
        self._signal = LazyStrategy(self)
        # self._on_init()  # event

    def get(self):
        # self._signal.on_get_val()
        # OR: self._signal('get_val')
        return self._g[self._uid]

    def __iter__(self):
        self._signal.on_iter_beg()
        for uid in self._g.neighbors(self._uid):
            # self._signal.on_iter_each()
            yield NodeProxy(self._g, uid)
        # self._signal.on_iter_end()

    @property
    def g(self):
        return self._g

    @property
    def uid(self):
        return self._uid


# THINK: if graph is empty -- run signal and generate first node as root
class GraphProxy(object):
    def __init__(self, g=None):
        self._g = g
        self._rootuid = None

    def __getitem__(self, uid):
        return NodeProxy(self._g, uid)

    # DECIDE: iterate only existing/cached nodes or traverse by all neighbors
    #   ALT:(compromise): manually trigger "cache all now"
    #     -- then you can traverse whole graph as all nodes will be cached
    def __iter__(self):
        for uid in self._g:
            yield NodeProxy(self._g, uid)

    def add_edge(self, np1, np2):
        self._g.add_edge(np1.uid, np2.uid)

    def add_object(self, obj):
        return NodeProxy(self._g, self._g.add_object(obj))

    def clear(self):
        return self._g.clear()

    # *************************************
    # NOTE:
    #   * generate first node if does not exists
    #   * insert into graph if container-backed and still not there
    #   * return from graph and wrap into proxy
    def get_root(self):
        return NodeProxy(self._g, self._rootuid)

    def set_root(self, obj):
        self._rootuid = self._g.add_object(obj)
        return NodeProxy(self._g, self._rootuid)

    def transform(self, tr_op):
        return tr_op(self)


class Transformation(object):
    def __init__(self):
        self._argv = None
        self._ctx = {}

    def set_args(self, argv):
        self._argv = argv

    def set_ctx(self, ctx):
        self._ctx = ctx

    # NOTE: node or [node] describing area where transformation applies
    def set_loci(self, loci):
        self._loci = loci

    def __call__(self, g):
        g.clear()
        g._root = g.add_object('.')
        for nm in proto.execute_0('ls'):
            g.add_edge(g.get_root(), g.add_object(nm))
