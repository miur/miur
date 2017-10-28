from . import proxy, provider


def run(argv):
    g = PersistentGraph(provider.FSTreeProvider('/etc/asciidoc')())
    proxy.print2lvl(g.get_root())


def run1(argv):
    g = provider.FSTreeProvider('/etc/asciidoc')()
    for n in g.neighbors(g.get_root()):
        print(g[n].name)
        for nn in g.neighbors(n):
            print('  ' + g[nn].name)


class PersistentNode(object):
    def __init__(self, g, uid):
        self._g = g
        self._uid = uid

    def __call__(self):
        return self._g[self._uid]

    def __iter__(self):
        for uid in self._g.neighbors(self._uid):
            yield PersistentNode(self._g, uid)


class PersistentGraph(object):
    def __init__(self, g):
        self._g = g

    def get_root(self):
        return PersistentNode(self._g, self._g.get_root())
