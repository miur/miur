#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
import logging
from collections import defaultdict

from . import dom

_log = logging.getLogger(__name__.split('.', 2)[1])

# https://bradfieldcs.com/algos/graphs/representing-a-graph/
# https://stackoverflow.com/questions/19472530/representing-graphs-data-structure-in-python
#   http://igraph.org/
#   http://networkx.github.io/
#   TRY:USE: https://networkx.github.io/documentation/latest/reference/classes/digraph.html#methods
#     => use complete graph library to understand necessary API and limitations
#   THINK: embed to miur to provide algorithms on graphs
#     * bake current complete fs state -- load necessary meta for all files
#     * apply alg to networkx graph imported from native baked graph
#     * mark some parts of graph as dirty/out-of-date by inotifywait events
#     * lazy reload dirty parts only when they appear in viewport

# ++++ https://en.wikipedia.org/wiki/Graph_rewriting
# https://en.wikipedia.org/wiki/Graph_(abstract_data_type)#Representations
# ++ https://en.wikipedia.org/wiki/Gremlin_(programming_language)
#   https://en.wikipedia.org/wiki/OrientDB
#   +? http://www.gstore-pku.com/en/

# IDEA: watch dir/tree
#   * disable = refresh content only manually
#   * silent = inotifywait and update content without questions
#   * mark = mark new files green, changed yellow, deleted red, replaced brown


# THINK: <> Provider <> Accessor <> Executor <> CommandPattern
#   * CommandPattern == lazy / deffered evaluation, binded values
#   * Executor == immediate results from vars passed to Interpreter
#   * Accessor == per-file / per-stat / per-param ways to aquire values
#     E.G. all accessors are underlying for specialized GraphProvider
#   * Provider == substitutable wrapper over service
#     E.G. FileSystemProvider = ShellProvider, SocketProvider, ProcessProvider
#   == FileSystemProvider(GraphProvider) -> {NativeAccessor|ShellAccessor}
#   = ShellAccessor -> {ShellProvider}
def run(argv):
    gp = GraphProxy(ObjectGraph())

    nps = gp.add_objects_from("abc")
    gp.add_edge(nps[0], nps[1])
    gp.add_edge(nps[1], nps[2])
    exe = gp.add_object(dom.ShellProvider('ls'))
    gp.add_edge(nps[2], exe)

    for np in gp:
        s = str(np.get()) + ' -> {' \
            + ', '.join(str(n.get()) for n in np) + '}'
        print(s)

    # n = g.getRoot()
    # NOTE: don't return edges container directly -- wrap in iterator
    #   <= because list may change any time
    # for e in n.getEdges():
    #     print(e)

# graph = node >> embeddable -- separate containers
# graph ifc :: shared for containers and evaluators
# layers -- pass graph to and froth
# graph as query/request/evaluator = AST = Lisp ?
# object/provider + accessor/adapter = service
# IDEA: construct Graph from NodeCompositeImpl -- which can contain any type
#   => substitute impl with NONE / Node / CachedNode / GraphNode unnoticed
#   => NEED: GraphNode.connectRoot(e) for e in edges
#       => to specify root (entry) point into graph from each edge viewpoint
# NOTE: substitute node keeping edges intact by change_node(uid, obj)


# NOTE: addr of each node is pair (graph, uid)
#   ALT: uid unique through all graphs for cross-references
def g_new_uid():
    g_new_uid._maxuid += 1
    return g_new_uid._maxuid


g_new_uid._maxuid = 0


## NOTE: use only if {_edges} will be amnested
# def g_new_euid(self):
#     g_new_euid._maxuid += 1
#     return g_new_euid._maxuid
# g_new_euid._maxuid = 0


# BAD: hard to determine necessary members
#   ~ dynamic map of attrs -- use protocol to query, list and acquire values
#       ? where I need such reflection ?
#   ~ hardcoded Node classes -- types are checked by python
# WARN: all Node/Edge must be invalidated when graph deleted
# NOTE: actually, they are ProxyNode
#   -- because they can acquire info from any backref data struct (self._g)
class INode(object):
    # attributes = {}  # NOTE: edges as one of attributes
    def __init__(self, g, uid):
        self._g = g
        self._uid = uid

    def __str__(self):
        return "node"


class IEdge(object):
    # attributes = {}  # NOTE: two nodes: 'beg/end' -- as attributes
    def __init__(self, g):
        self._g = g

    def __str__(self):
        return "edge"


# ifc, data-only, code-only, cached code results ==> hard to split
# ALT: composition instead of inheritance
#   E.G. "GraphNode" adapter to contain whole virtual graphs
#   * GraphNode is monolith -- you can't acquire access to subnodes from here
#   * GraphNode connects external edges with some internal input nodes of graph
#   * GraphNode is membrane separating outer and inner structs


# MAYBE: container inherited from GraphProvider
# attrs == state
#   + memory (history)
#   + knowledge about other nodes (environment / surrounding world)
# edges == IO monad :: there is no more IO to query info from Node
#   = bus / pipe -- mirror some piece of data from one end to another
#   + modify/convert content when transferring
#   + EdgeGraph -- encapsulates whole modification logic

# Both any node or edge could be replaced by graph
#   => single uid counter <= no need to separate ID-space for nodes and edges
#   => BUT: separate containers for nodes and edges for enumeration optimizations

# NOTE: adding node, its associated value and edge between nodes -- all are independent separate operations
#   * layer of graph / relations
#   * layer of object values
#   * layer of proxy accessors to virtual/generated graphs and values
# CHG: rename => GraphContainer (uid -> uid)
class BaseGraph(object):
    # nodes + attrs
    # edges + attrs
    def __init__(self):
        # BAD: separate containers
        #   * excessive lookups per each hashmap
        #   * distance in memory -> cache miss
        #   * must be optimized for graphs which are "mostly tree" (at least)
        #     => nr_edges ~>= nr_nodes - 1
        #     => traversing nodes one-by-one => need adjacency info
        ## NEED: for edges pointing to non-existent uids
        #   ALSO: traversing/filtering list of all nodes
        #       -- currently not in priority
        # self._edges = {}
        # THINK: is there need for sep _attrs field ?
        #   * general/common graph-related attrs can be stored directly in graph
        #       independent from node/edge object in graph itself
        #   => has sense if graph embodies network/media of some kind
        self.clear()

    def add_node(self, uid, obj=None):
        if uid in self._nodes:
            raise
        self._nodes[uid] = obj
        # self._neighbors[uid] = set()  # USE: = .setdefault(uid, set())
        # NOTE: if there are dangling edges which pointed to this uid
        #   => find them and insert into _neighbors
        # BET? prohibit dangling edges => easier ALG, lesser overhead
        #   BET! always keep edges "placeholder" by _neighbors but don't insert into _nodes to test existence

    # NOTE: I need edges -- because there is no place to embed edge value
    #   => it may be emulated by two edges and node
    #   * values may affect network traversing algs itself
    #   * _edges with values may be added postfactum when necessary
    #       node1 -> _neighbors -> euid1-2 -> edge1-2 -> uid2 -> node2
    def add_edge(self, uid1, uid2):
        # uid = ObjectGraph._new_uid()
        # BAD: custom chosen data struct
        #   => may discard _edges altogether and keep only _neighbors
        #   BUT: you can generate whole node / graph on the other side simply by
        #   accessing dangling (lazy) edge -- you don't know where you point to
        #   until generation succeeds
        #   ALT: dangling ends of edges may point to special node with uid=None or uid=0
        #       => it allows to enumerate all free edges and delay acquiring actual uid for other end
        #       => then edges pointing to nonexistent nodes are truly dangling and are system errors
        #   Actually, _neighbors may contain node to enlist connections, but _nodes won't contain even =None
        # self._edges[uid] = (uid1, uid2)
        # BAD: directly saves neighbor uid instead of intermediate 'edge'
        #   => must save ptrs to 'edge' to be able to be replaced by 'graph'
        # BAD: how to connect each neighbor with its own point in GraphNode ?
        #   => only GraphNode may decide it depending on who edge points to
        #     -> [_] DEV pass neighbor node directly into add_edge() of GraphNode
        #   => THINK: passing edge is not exclusive -> you still need _neighbors
        #     when replacing SimpleNode by GraphNode and back to re-supply whole
        #     list of _neighbors to constructor
        #     ALSO you need full fledged object of Edge -- to remain connected
        #     on one side (surrounding graph knowledge) when replacing edges
        #     connect-point to another node
        # BAD: _neighbors don't support multi-edge connection between nodes
        #   BUT: it supports self-edge
        # HACK: using edge-object you can find who connects to you
        #   => so you can reconnect edges to another uid on node replace
        #   * directed edge by itself in _neighbors doesn't allow this
        #   * pair of directed edges requires to ensure invariants
        #   * sometimes you need only single directed edge (e.g. symlink)
        #   CHECK: no need to reconnect -- replace entity in node itself
        self._neighbors[uid1].add(uid2)
        self._neighbors[uid2].add(uid1)
        # NEED: cache added node in adjacency set of each node
        # return uid

    def neighbors(self, uid):
        return iter(self._neighbors[uid])

    def clear(self):
        self._nodes = {}
        self._neighbors = defaultdict(set)  # cached adjacency

    ## Convenience methods
    # Prefer uids of nodes for default operations
    def __iter__(self):
        return iter(self._nodes)

    def __len__(self):
        return len(self._nodes)

    def __contains__(self, uid):
        return uid in self._nodes

    def __getitem__(self, uid):
        return self._nodes[uid]

    def __setitem__(self, uid, obj):
        self._nodes[uid] = obj

    def __delitem__(self, uid):
        # THINK: also delete part of network ?
        #   => diff kind of "delete"
        #   * existence in _nodes
        #   * value vs None
        #   * edges as network knowledge
        #   * memory knowledge -- this uid from all vals of whole graph
        del self._nodes[uid]


# API accepts id (instead of objects) and returns objects itself
# CHG: rename => GraphAdapter (because it pairs interface with container)
## Convenience methods (uid -> obj) -- can be merged to container or dropped
class ObjectGraph(BaseGraph):
    def __init__(self, _new_uid=g_new_uid):
        self._new_uid = _new_uid
        super().__init__()

    # high-level api -- relocate in its own ObjectGraph superstructure
    #   MAYBE: aggregate instead of inherit ?
    def add_object(self, obj):
        uid = self._new_uid()
        self.add_node(uid)
        self[uid] = obj
        return uid

    # DEV: use as mix-in :: aggregate both (ContainerGraph + ObjectGraph)
    #   BUT: in case of GraphProxy them may require additional logic anyway
    #   BET? independent global functions to operate on IGraph
    #    => can't be optimized for range operations CHECK if needed
    #     == gen list of uids
    def add_objects_from(self, iterable):
        return [self.add_object(obj) for obj in iterable]

    ## WARN: don't return objects instead uid -> it mutilates API
    # def __iter__(self):
    #     return iter(self._nodes)


# NOTE:
# * weak_ptr => may become nonexistent => exception | ret node.val itself
# * may directly provide INode interface, locking node object on access
#   => BUT: need specialized proxy per each INode ifc => can't be general
#   NEED: sep method query per attr or attr range: name, content, stats, ...
#     == Accessor / CachingAccessor with methods per property
#     BAD: edges are also node property or independent ?
#       << edges are outside of node value, being part of graph._neighbors
#       << but edges can be queried from node accessor, E.G. files in dir
# * provide directly NodeProxy instead of EdgeProxy
#   - preview for current dir may require previews for all entries at once
#   - can't choose clear attrs subset to delegate to edge (e.g. name, stats)
class NodeProxy(object):
    def __init__(self, g, uid):
        self._g = g
        self._uid = uid

    def get(self):
        return self._g[self._uid]

    # ALT: ret edges and walk by euid
    def __iter__(self):
        for uid in self._g.neighbors(self._uid):
            yield NodeProxy(self._g, uid)

    @property
    def uid(self):
        return self._uid


# API accepts/returns proxy instance of accessors to IdGraph
#   * each proxy aggregates _uid and _g to be able to operate on itself
#   == weak_ptr == actual object inside Graph may be destroyed any time
# NOTE: layer which combines heterogeneous graphs in single namespace
class GraphProxy(object):
    def __init__(self, g):
        self._g = g

    def __getitem__(self, uid):
        return NodeProxy(self._g, uid)

    # NEED: gen iter_range(1, 100) or g[1:100] for scrolling really long lists
    #   THINK: update only viewport when random files add/del
    #   NEED: continuous infinite / stream iterator E.G. lines in "less" pager
    def __iter__(self):
        for uid in self._g:
            yield NodeProxy(self._g, uid)

    # SEE: convenience funcs from Networkx
    #   add_edges_from([(1,2)..])
    #   add_neighbours_from(1, [2..]) == add_star_from([1,2..])
    #   add_path_from([1,2,3..])
    def add_edge(self, np1, np2):
        self._g.add_edge(np1.uid, np2.uid)

    def add_object(self, obj):
        return NodeProxy(self._g, self._g.add_object(obj))

    def add_objects_from(self, iterable):
        return [self.add_object(obj) for obj in iterable]

    def add_star_from(self, rootnode, iterable):
        for newnode in self.add_objects_from(iterable):
            self.add_edge(rootnode, newnode)


# Wrapper classes extend base container by shortcuts
#   E.G. add_node_connected_to(node, [neighbour_nodes])
#     == add_node() + add_edges_from()
# THINK: replace dedicated "root" concept by aquired "entry point for edge" from GraphNode
#   => must be aware of all edges and attach each connection separately
class RootedGraph(GraphProxy):
    def __init__(self, rootid=None, **kw):
        self._rootid = rootid
        super().__init__(**kw)

    def getRoot(self):
        """ Construct node object with backref to graph """
        return self.get_node(self._rootid)
