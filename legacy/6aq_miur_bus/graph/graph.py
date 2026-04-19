#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
from collections import defaultdict


def g_new_uid():
    g_new_uid._maxuid += 1
    return g_new_uid._maxuid


g_new_uid._maxuid = 0


def print2lvl(g, ru):
    print('[' + g[ru].name + ']')
    for u in g.neighbors(ru):
        print(g[u].name)
        for uu in g.neighbors(u):
            print('  ' + g[uu].name)


class IGraphRead(object):
    def __getitem__(self, uid):
        raise NotImplementedError()

    def neighbors(self, uid):
        raise NotImplementedError()


class IGraphWrite(object):
    def clear(self):
        raise NotImplementedError()

    def __setitem__(self, uid, obj):
        """ Set value for new or existing node """
        raise NotImplementedError()

    def add_arrow(self, b_uid, e_uid):
        raise NotImplementedError()


class IGraphContainer(IGraphRead, IGraphWrite):
    pass


# NOTE: container has two sep ifc -- 'get' and 'set'
#   => immediate graph is container-like but has only 'get' ifc
#    ~ maybe it can support part of 'set' ifc such as set_root()
class GraphContainer(IGraphContainer):
    def __init__(self):
        self.clear()

    def clear(self):
        self._nodes = {}
        self._neighbors = defaultdict(set)  # cached adjacency

    def __getitem__(self, uid):
        return self._nodes[uid]

    def __setitem__(self, uid, obj):
        """ Set value for new or existing node """
        self._nodes[uid] = obj

    def add_arrow(self, b_uid, e_uid):
        if b_uid not in self._nodes or e_uid not in self._nodes:
            raise KeyError(b_uid, e_uid)
        if b_uid in self._neighbors and e_uid in self._neighbors[b_uid]:
            raise LookupError(b_uid, e_uid)
        self._neighbors[b_uid].add(e_uid)

    def neighbors(self, uid):
        return self._neighbors[uid]


class ExtendedGraphContainer(GraphContainer):
    # BAD: impossible for ImmediateGraph
    def __iter__(self):
        return iter(self._nodes)

    # BAD: impossible for ImmediateGraph
    def __len__(self):
        return len(self._nodes)

    def add_edge(self, uid1, uid2):
        self.add_arrow(uid1, uid2)
        self.add_arrow(uid2, uid1)

    def add_object(self, obj):
        uid = g_new_uid()
        if uid in self._nodes:
            raise KeyError(uid)
        self[uid] = obj
        return uid


class RootedGraphContainer(ExtendedGraphContainer):
    def __init__(self):
        super().__init__()
        self._rootuid = None

    def get_root(self):
        return self._rootuid

    def set_root(self, uid):
        self._rootuid = uid
