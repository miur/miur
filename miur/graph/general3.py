# INFO: indirect NodeProxy -> GraphProxy -> NodeContainer

from . import graph, entity, proxy, provider, transform


def run(argv):
    gp = OndemandGraphProxy(GraphContainer())
    rp = gp.add_object(None)
    gp.set_root(rp._uid)  # BAD api
    # gp.set_strategy(rp, AccumulateGenerator())
    proxy.print2lvl(gp.get_root())


def run2(argv):
    g = provider.FSTreeProvider('/etc/asciidoc')()
    gp = PersistentGraphProxy(g)
    proxy.print2lvl(gp.get_root())


def run1(argv):
    g = ImmediateGraphProxy()
    g.set_root(entity.DirEntity('/etc/asciidoc'))
    proxy.print2lvl(g.get_root())


# NOTE: uid may be anything
# => it's simply unique identifier which allows to distinguish and access
# individual graph nodes by graph ifc
#   * path for GraphOverlay
#   * hash for RemoteGraph
#   * number for persistent GraphContainer
#   * NodeContainer object for ImmediateGraph
# NOTE: NodeProxy only must know ifc of underlying GraphProxy
class NodeProxy(object):
    def __init__(self, gp, uid):
        self._gp = gp
        self._uid = uid

    def __call__(self):
        return self._gp[self._uid]

    def __iter__(self):
        yield from self._gp.neighbors(self._uid)


# INFO: NodeContainer can't store "uid" <= because of ImmediateGraph
#   ?=> ImmediateGraph has no strategy -- OR it is strategy by itself ?
class NodeContainer(object):
    def __init__(self, obj, strategy=None):
        self._obj = obj
        self._neighbors = set()
        self._strategy = strategy
        # NOTE: transformation must be parameterizing the strategy
        self._transform = None

    def set_strategy(self, strategy):
        self._strategy = strategy

    def __call__(self, g):
        return self._obj

    def neighbors(self, g):
        # NOTE: depends on obj ifc -- to fetch list of linked objects
        # try:
        #     yield from self(g)
        # except TypeError:
        #     return
        # self._strategy('neighbors', g, self)
        return self._neighbors

    def add_neighbor(self, g, uid):
        if uid in self._neighbors:
            raise LookupError(uid)
        self._neighbors.add(uid)
        return uid


# INFO: uid == nc :: NodeContainer itself
# USAGE: Factory -- used only to wrap objects into NodeProxy
#   NEED: private NodeProxy constructor
# MAYBE: even immediate graph must store links to all currently existing
#   NodeContainer/NodeProxy ?
#   ALT: log these links inside ctor/dtor of NodeContainer
#     => otherwise nc must call 'del g[uid]' in dtor to remove ref from graph
class ImmediateGraphProxy(object):
    def __init__(self, g=None):
        pass

    def __getitem__(self, nc):
        return nc(self)

    # BAD: incoherent api -- add_object return np instead of nc
    def neighbors(self, nc):
        return (self.add_object(o) for o in nc.neighbors(self))

    def add_object(self, obj):
        return NodeProxy(self, NodeContainer(obj))

    # BAD: incoherent api -- got obj instead of nc
    def set_root(self, obj):
        self._root_np = self.add_object(obj)

    def get_root(self):
        return self._root_np


class PersistentGraphProxy(object):
    def __init__(self, g):
        self._g = g

    def __getitem__(self, uid):
        return self._g[uid]

    def neighbors(self, uid):
        return (NodeProxy(self, u) for u in self._g.neighbors(uid))

    def add_object(self, obj):
        return NodeProxy(self, self._g.add_object(obj))

    def get_root(self):
        return NodeProxy(self, self._g.get_root())


# BAD: returns nodeproxy but accepts uids -- !no way to convert!
class OndemandGraphProxy(object):
    def __init__(self, g):
        self._g = g

    def set_root(self, uid):
        self._rootuid = uid

    def get_root(self):
        return NodeProxy(self, self._rootuid)

    def set_strategy(self, uid, strategy):
        return self._g.set_strategy(uid, strategy)

    # Strategy is part of graph itself
    #   => you can't save uid inside NodeContainer anyway
    def _lazygen(self, g, uid):
        # TEMP: hardcoded strategy
        if self._g[uid] is None:
            aug = provider.FSTreeProvider('/etc/asciidoc')()
            transform.NodeSuperimposeTr()(g, uid, aug)

    def __getitem__(self, uid):
        self._lazygen(self._g, uid)
        return self._g[uid]

    def __setitem__(self, uid, obj):
        self._g[uid] = obj

    def neighbors(self, uid):
        self._lazygen(self._g, uid)
        return (NodeProxy(self, u) for u in self._g.neighbors(uid))

    def add_arrow(self, b_uid, e_uid):
        return self._g.add_arrow(b_uid, e_uid)

    def add_object(self, obj):
        return NodeProxy(self, self._g.add_object(obj))


class GraphContainer(object):
    def __init__(self):
        self.clear()

    def clear(self):
        self._nodes = {}

    def __getitem__(self, uid):
        return self._nodes[uid](self)

    def __setitem__(self, uid, obj):
        self._nodes[uid] = NodeContainer(obj)

    def neighbors(self, uid):
        return self._nodes[uid].neighbors(self)

    def add_arrow(self, b_uid, e_uid):
        if e_uid not in self._nodes:
            raise KeyError(e_uid)
        self._nodes[b_uid].add_neighbor(self, e_uid)

    def add_object(self, obj):
        uid = graph.g_new_uid()
        if uid in self._nodes:
            raise KeyError(uid)
        self[uid] = obj
        return uid

    def set_strategy(self, uid, strategy):
        return self._nodes[uid].set_strategy(strategy)
