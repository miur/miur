from . import graph, proxy, provider, transform


def run(argv):
    g = graph.RootedGraphContainer()
    g.set_root(g.add_object(None))

    rp = SignalNodeProxy(g, g.get_root())
    rp.set_strategy(SubstituteStrategy())

    for n in rp:
        print(n().name)
        for nn in n:
            print('  ' + nn().name)


# ALT: emit to underlying graph -> replace graph
# NOTE: here _strategy is default strategy per all signals
class SignalNodeProxy(proxy.NodeProxy):
    def set_strategy(self, strategy):
        self._strategy = strategy

    def _emit(self, method):
        if self._strategy:
            self._strategy(method, self)

    def __call__(self):
        self._emit('__getitem__')
        return super().__call__()

    def __iter__(self):
        self._emit('neighbors')
        return super().__iter__()


# NOTE: hardcoded one-time Strategy :: signal -> Tr
#   ? from where graph provider acquired and where stored ?
class SubstituteStrategy(object):
    def __call__(self, method, proxy):
        gpr = provider.FSTreeProvider('/etc/asciidoc')
        tr = transform.NodeSuperimposeTr()
        tr(proxy._g, proxy._uid, gpr())
        proxy._callback = None
