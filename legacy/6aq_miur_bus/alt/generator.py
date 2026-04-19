#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
# TODO:(py3.4):USE: pathlib instead of os.path to manipulate pure paths
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


def print_graph(node, seen, depth=0):
    p = node.get()
    if p in seen:
        return
    seen.add(p)
    print('  '*depth + fs.basename(p))
    for n in node:
        print_graph(n, seen, depth + 1)


def run(argv):
    g = FindWalkerGraph('/etc/asciidoc')
    print_graph(g.getRoot(), set())
    # for node in g.getRoot():
    #     print(node.get())
    #     for n in node:
    #         print('  ' + n.get())


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
# Persistent on-demand
#   * nothing -> all
#   * created/destroyed at once
class FindNode(object):
    def __init__(self, cmd, path):
        self._cmd = cmd
        self._path = path
        self._g = None
        self._uid = None

    # NEED: more node attrs accessors :: content, stats, etc
    def get(self):
        return self._path

    # def _paths2node(self, paths):
    #     g = graph.GraphProxy(graph.ObjectGraph())
    #     root = g.add_object(self._path)
    #     g.add_star_from(root, paths)
    #     self._g = g

    def _paths2graph(self, paths):
        g = graph.ObjectGraph()
        self._uid = g.add_object(self._path)
        cache = {'': self._uid}
        for p in paths:
            if p in cache:
                continue
            cache[p] = g.add_object(p)
            while '' != p:
                pp = fs.dirname(p)
                if pp in cache:
                    g.add_edge(cache[pp], cache[p])
                    break
                cache[pp] = g.add_object(pp)
                g.add_edge(cache[pp], cache[p])
                p = pp
        return g

    def __iter__(self):
        if self._g is None:
            self._g = self._paths2graph(self._cmd(self._path))
        for uid in self._g.neighbors(self._uid):
            yield graph.NodeProxy(self._g, uid)


class FindWalkerGraph(object):
    def __init__(self, rootpath='/'):
        self._rootpath = rootpath
        self._cmd = FindCommand()

    def getRoot(self):
        return FindNode(self._cmd, self._rootpath)
