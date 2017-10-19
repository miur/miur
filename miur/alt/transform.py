""" Transformation
 * Graph is coordinate/uid space
 * Attrs are binded to coords (uid), regions (uid.rad(2)) or clusters (set{uid})
 * GraphProxy -> NodeProxy -> {Events + Data}
"""

from . import proto, graph


def run(argv):
    g = GraphProxy(graph.ObjectGraph())  # empty
    Transformation()(g)
    # for n in g.getRoot():
    for n in g:
        print(n.get())


class NodeProxy(object):
    def __init__(self, g, uid):
        self._g = g
        self._uid = uid
        # self._on_init()  # event

    def get(self):
        self._on_get_val()
        return self._g[self._uid]

    def __iter__(self):
        self._on_iter_beg()
        for uid in self._g.neighbors(self._uid):
            self._on_iter_each()
            yield NodeProxy(self._g, uid)
        self._on_iter_end()

    @property
    def uid(self):
        return self._uid

    def _on_get_val(self):
        print("***")

    def _on_iter_each(self):
        print("*")
        self._g.transform(self.get().transformation)  # XXX???

    def _on_iter_beg(self): print(">")

    def _on_iter_end(self): print("<")


class GraphProxy(object):
    def __init__(self, g):
        self._g = g
        self._root = None

    def __getitem__(self, uid):
        return NodeProxy(self._g, uid)

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
    def getRoot(self):
        return self._root

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

    def __call__(self, g):
        g.clear()
        g._root = g.add_object('.')
        for nm in proto.execute_0('ls'):
            g.add_edge(g.getRoot(), g.add_object(nm))
