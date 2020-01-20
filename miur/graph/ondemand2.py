#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
from . import graph, provider, transform


def run(argv):
    g = graph.RootedGraphContainer()
    g.set_root(g.add_object(None))
    gp = OndemandGraph(g)
    ru = gp.get_root()
    graph.print2lvl(gp, ru)


# MAYBE: NodeProxy isn't necessary at all -- move everything to GraphProxy ?
#   * refer to nodes directly by their uids and graph ref
#   * events can be emitted directly from GraphProxy
#   * NodeProxy must be superstructure over GraphProxy, not GraphContainer
#     => so, it can be impl afterwards -- only to unite ifc, uid and graph ref
#     E.G. np=NodeProxy(g, g.get_root()) && for n in g.neighbors(): print(n())
#   ? THINK heterogeneous nodes with ondemand strategy ?
class OndemandGraph(object):
    def __init__(self, g):
        self._g = g

    def get_root(self):
        return self._g.get_root()

    def __getitem__(self, uid):
        # BAD: populates whole graph BUT:NEED:(usually): load this entity only
        self._lazygen(uid)
        return self._g[uid]

    def neighbors(self, uid):
        self._lazygen(uid)
        return self._g.neighbors(uid)

    # BAD: graph-wide hardcoded gen strategy and trigger condition
    def _lazygen(self, uid):
        if self._g[uid] is None:
            aug = provider.FSTreeProvider('/etc/asciidoc')()
            transform.NodeSuperimposeTr()(self._g, uid, aug)
