import os.path as fs
from . import proto, graph

# GraphGenerator(RootedGraph)
#   * embeds ALG to produce neighbors NodeProxy from current position
#   * transient iterative generation / fractal
#   * may produce large graphs/clusters at once
#     - range/path long query
#     - parse and cache E.G. AST
## CASE:("find")
#   * flat graph == single node with edges
#   * isolated graph == cached graph with single NodeGenerator
#   * iterative graph == run "find" on current dir only
#     AND: run recursive only if "cache all" / "immediate snapshot"


def run1(argv):
    g = DirWalkerGraph('/etc/asciidoc')
    for node in g.getRoot():
        print(node.get())
    # for node in g.getRoot():
    #     s = str(node.get()) + ' -> {' \
    #         + ', '.join(str(n.get()) for n in node) + '}'
    #     print(s)


# BUG: returns objects instead of values
def run(argv):
    g = FindWalkerGraph('/etc/asciidoc')
    for node in g.getRoot():
        print(node.get())


# TRY: replace by "functools.bind()"
class DirCommand(object):
    def __init__(self):
        self._cmd = ['find', '-L', '-mindepth', '1', '-maxdepth', '1', '-printf', r'%P\0']

    def __call__(self, path):
        lst = proto.execute_0(self._cmd[:2] + [path] + self._cmd[2:])
        return [] if lst is None else lst


class DirNode(object):
    def __init__(self, cmd, path):
        self._cmd = cmd
        self._path = path

    def get(self):
        return self._path

    def __iter__(self):
        for nm in self._cmd(self._path):
            yield DirNode(self._cmd, fs.join(self._path, nm))


class DirWalkerGraph(object):
    def __init__(self, rootpath='/'):
        self._rootpath = rootpath
        self._cmd = DirCommand()

    def getRoot(self):
        return DirNode(self._cmd, self._rootpath)


#############


class FindCommand(object):
    def __init__(self):
        self._cmd = ['find', '-L', '-mindepth', '1', '-type', 'f', '-printf', r'%P\0']

    def __call__(self, path):
        lst = proto.execute_0(self._cmd[:2] + [path] + self._cmd[2:])
        return [] if lst is None else lst


# NOTE: on access generates graph and caches it in isolated GraphContainer
class FindNode(object):
    def __init__(self, cmd, path):
        self._cmd = cmd
        self._path = path
        self._g = None

    # NEED: more node attrs accessors :: content, stats, etc
    def get(self):
        return self._path

    # def _paths2node(self, paths):
    #     g = graph.GraphProxy(graph.ObjectGraph())
    #     root = g.add_object(self._path)
    #     g.add_star_from(root, paths)
    #     self._g = g

    def _paths2graph(self, paths):
        g = graph.GraphProxy(graph.ObjectGraph())
        path2uid = {'': g.add_object(self._path)}
        for pp in paths:
            p = pp
            while p not in path2uid and '' != p:
                path2uid[p] = g.add_object(p)
                p = fs.dirname(p)
            g.add_edge(path2uid[fs.dirname(p)], path2uid[pp])
        return g

    def __iter__(self):
        if self._g is None:
            self._g = self._paths2graph(self._cmd(self._path))
        for uid in self._g:
            yield graph.NodeProxy(self._g, uid)


class FindWalkerGraph(object):
    def __init__(self, rootpath='/'):
        self._rootpath = rootpath
        self._cmd = FindCommand()

    def getRoot(self):
        return FindNode(self._cmd, self._rootpath)
