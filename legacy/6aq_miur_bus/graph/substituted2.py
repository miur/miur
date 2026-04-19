#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
from . import graph, proxy, provider, transform


def run(argv):
    g = LazySlotGraphContainer()
    g.set_root(g.add_object(None))
    g.set_lazy(g.get_root())

    gd = SignalDecorator(g, g.slot)
    rp = proxy.NodeProxy(gd, gd.get_root())
    proxy.print2lvl(rp)


# BAD: replacement must be on per-node basis
# BAD: graph inheriting won't work
#   => otherwise you must copy graph data to regular G
#   => use decoration and substitute underlying container into proxy
#   BAD: we must substitute graph inside SignalDecorator instead of proxy
#     TRY backward propagation Proxy->Lazy->Signal->Container
#       << block on signal and call Lazy again: Signal(container, lazy.slot)
# BAD: to subs _g inside proxy it must pass "self" to signal handler
#   => signal emitting ifc must be inside NodeProxy


class SlotGraphContainer(graph.RootedGraphContainer):
    def slot(self, method, uid):
        print(method, uid)


# THINK: Strategy :: signal -> Tr
#   ? from where graph provider acquired ?
class LazySlotGraphContainer(SlotGraphContainer):
    def slot(self, method, uid):
        if uid == self._lazy and method in ['__getitem__', 'neighbors']:
            gpr = provider.FSTreeProvider('/etc/asciidoc')
            tr = transform.NodeSuperimposeTr()
            tr(self, uid, gpr())
            self._lazy = None
        # super().slot(method, uid)

    def set_lazy(self, uid):
        self._lazy = uid


# WARN: use SignalDecorator instead of inherited SignalGraphContiner
#   * signals must be triggered for any underlying graph type
#   * underlying graph must be replaceable ondemand
# TODO: inherit ifc IGraph = IGraphGet + IGraphSet
class SignalDecorator(object):
    def __init__(self, g, callback):
        self._g = g
        self._emit = callback

    def get_root(self):
        return self._g.get_root()

    def __getitem__(self, uid):
        # OR self._on_get_item() for name-checking by Python
        self._emit('__getitem__', uid)
        return self._g[uid]

    def __setitem__(self, uid, obj):
        self._g[uid] = obj

    def neighbors(self, uid):
        self._emit('neighbors', uid)
        return self._g.neighbors(uid)

    def add_object(self, obj):
        return self._g.add_object(obj)

    def add_arrow(self, b_uid, e_uid):
        return self._g.add_arrow(b_uid, e_uid)
