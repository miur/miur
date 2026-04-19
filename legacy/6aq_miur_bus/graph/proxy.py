#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#

# NOTE: all operations with nodes are conducted through NodeProxy
#   * NodeProxy may point to any type of graph -- Immediate, Persistent, etc
#   * If you need to replace underlying container of NodeProxy in runtime
#     -- then you need sep accessors per each type of unerlying graph
#     MAYBE: accessors have ifc equal to NodeContainer
#       => then you can directly connect NodeProxy with NodeContainer without
#       intermediate graph (which doesn't have container for Immediate anyway)
# ! uid is meaningless for ImmediateGraph
#   ~ directly connect NodeProxy->NodeContainer or ->Accessor->GraphContainer
#   ~ always pass NodeProxy->{Immediate,Persistent}GraphProxy


def print2lvl(rp):
    print('[' + rp().name + ']')
    for n in rp:
        print(n().name)
        for nn in n:
            print('  ' + nn().name)


class NodeProxy(object):
    def __init__(self, g, uid):
        self._g = g
        self._uid = uid

    def __call__(self):
        return self._g[self._uid]

    def __iter__(self):
        for uid in self._g.neighbors(self._uid):
            yield NodeProxy(self._g, uid)
