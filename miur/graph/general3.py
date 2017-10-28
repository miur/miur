# INFO: indirect NodeProxy -> GraphProxy -> NodeContainer

from . import entity, proxy, provider


def run(argv):
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


class NodeContainer(object):
    def __init__(self, obj):
        self._obj = obj

    def __call__(self, g):
        return self._obj

    def neighbors(self, g):
        # NOTE: depends on obj ifc -- to fetch list of linked objects
        try:
            yield from self(g)
        except TypeError:
            return


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
