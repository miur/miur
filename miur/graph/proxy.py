# NOTE: all operations with nodes are conducted through NodeProxy
#   * NodeProxy may point to any type of graph -- Immediate, Persistent, etc
#   * If you need to replace underlying container of NodeProxy in runtime
#     -- then you need sep accessors per each type of unerlying graph
#     MAYBE: accessors have ifc equal to NodeContainer
#       => then you can directly connect NodeProxy with NodeContainer without
#       intermediate graph (which doesn't have container for Immediate anyway)


class NodeProxy(object):
    def __init__(self, g, uid):
        self._g = g
        self._uid = uid

    def __call__(self):
        return self._g[self._uid]

    def __iter__(self):
        for uid in self._g.neighbors(self._uid):
            yield NodeProxy(self._g, uid)
