# INFO: direct NodeProxy -> NodeContainer

from . import entity, proxy


# ? how to create first NodeProxy for ImmediateGraph ?
def run(argv):
    g = ImmediateGraphProxy()
    rp = g.add_object(entity.DirEntity('/etc/asciidoc'))
    proxy.print2lvl(rp)


# NOTE: works only with GraphProxy -- because ImmediateGraph has no uid
# INFO: Encapsulates accessor (graph, uid) or (nc)
class NodeProxy(object):
    def __init__(self, gp, nc):
        self._gp = gp
        self._nc = nc

    def __call__(self):
        return self._nc(None)

    def __iter__(self):
        yield from self._nc.neighbors(self._gp)


# INFO: Encapsulates (obj, neighbors) -- ?no link to concrete graph?
#   BUT: when it contains self._neighbors container -- it must contain uids
#       and uids have sense only inside specific graph
class NodeContainer(object):
    def __init__(self, obj):
        self._obj = obj

    def __call__(self, g):
        return self._obj

    def neighbors(self, g):
        try:
            it = iter(self(g))
        except TypeError:
            return
        else:
            for o in it:
                yield g.add_object(o)


# USAGE: Factory -- used only to wrap objects into NodeProxy
class ImmediateGraphProxy(object):
    def add_object(self, obj):
        return NodeProxy(self, NodeContainer(obj))
