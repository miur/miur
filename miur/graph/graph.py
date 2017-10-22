from collections import defaultdict


def g_new_uid():
    g_new_uid._maxuid += 1
    return g_new_uid._maxuid


g_new_uid._maxuid = 0


class GraphContainer(object):
    def __init__(self):
        self.clear()

    def clear(self):
        self._nodes = {}
        self._neighbors = defaultdict(set)  # cached adjacency

    def __getitem__(self, uid):
        return self._nodes[uid]

    def __setitem__(self, uid, obj):
        if uid in self._nodes:
            raise
        self._nodes[uid] = obj

    def add_object(self, obj):
        uid = g_new_uid()
        self[uid] = obj
        return uid

    def add_edge(self, uid1, uid2):
        self._neighbors[uid1].add(uid2)
        self._neighbors[uid2].add(uid1)

    def neighbors(self, uid):
        return self._neighbors[uid]


class RootedGraphContainer(GraphContainer):
    def __init__(self):
        super().__init__()
        self._rootuid = None

    def get_root(self):
        return self._rootuid

    def set_root(self, uid):
        self._rootuid = uid
