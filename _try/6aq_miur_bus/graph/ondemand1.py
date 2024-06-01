#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
from . import graph, proxy, provider


def run(argv):
    g = graph.RootedGraphContainer()
    g.set_root(g.add_object(None))
    gp = OndemandGraph(g)
    proxy.print2lvl(gp.get_root())


# TRY: replace proxy and all attrs accessors by weakptr
#   https://docs.python.org/3/library/weakref.html#weakref.proxy
class OndemandNode(object):
    def __init__(self, g, uid):
        self._g = g
        self._uid = uid

    # HACK: with global uid generator -- simply copy nodes/edges in graph
    #   => with GUG all graphs are subgraphs of single global id space
    #   BAD: you can't generate+send graphs from remote
    def lazygen(self):
        if self._g[self._uid] is not None:
            return
        aug = provider.FSTreeProvider('/etc/asciidoc')()
        conv = {}
        # NOTE: superimpose aug.get_root() with self._uid
        #   => WARN: can't simply connect by edge, otherwise entity remains None
        for uid in aug:
            if uid == aug.get_root():
                self._g[self._uid] = aug[uid]
                conv[uid] = self._uid
            else:
                conv[uid] = self._g.add_object(aug[uid])
        # ERR: edges with dangling ends (w/o node) raise error
        # NOTE: same edge contained in two _neighbors => copy sep arrows
        for uid in aug:
            for edge in aug.neighbors(uid):
                self._g.add_arrow(conv[uid], conv[edge])
        print(len(self._g))

    def __call__(self):
        self.lazygen()
        return self._g[self._uid]

    # THINK: move iter() from NodeProxy to graph.neighbors()
    #   => therefore node will represent only entity itself by weakref
    #   => graph controls neighbors in centralized manner
    #   ? how to iterate heterogeneous nodes with different graph strategy ?
    #   ? how to substitute graph for nodes when iterating ?
    #     => GraphProxy refers to whole graph, but only choosen nodes have
    #     ondemand strategy, so you can't replace whole graph by lazygen()
    #   BAD: NodeProxy don't know about GraphProxy
    def __iter__(self):
        self.lazygen()
        for uid in self._g.neighbors(self._uid):
            yield OndemandNode(self._g, uid)


class OndemandGraph(object):
    def __init__(self, g):
        self._g = g

    def get_root(self):
        return OndemandNode(self._g, self._g.get_root())
