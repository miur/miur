import logging

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


def run(argv):
    g = RootedGraph()
    n = g.getRoot()
    # NOTE: don't return edges container directly -- wrap in iterator
    #   <= because list may change any time
    for e in n.getEdges():
        print(e)

# graph = node >> embeddable -- separate containers
# graph ifc :: shared for containers and evaluators
# layers -- pass graph to and froth
# graph as query/request/evaluator = AST = Lisp ?
# object/provider + accessor/adapter = service
# IDEA: construct Graph from NodeCompositeImpl -- which can contain any type
#   => substitute impl with NONE / Node / CachedNode / GraphNode unnoticed


# BAD: hard to determine necessary members
#   ~ dynamic map of attrs -- use protocol to query, list and aquire values
#       ? where I need such reflection ?
#   ~ hardcoded Node classes -- types are checked by python
# WARN: all Node/Edge must be invalidated when graph deleted
# NOTE: actually, they are ProxyNode
#   -- because they can acquire info from any backref data struct (self._g)
class INode(object):
    # attributes = {}  # NOTE: edges as one of attributes
    def __init__(self, g):
        self._g = g
        self._edges = [IEdge(self._g), IEdge(self._g)]  # TEMP:REM

    def __str__(self):
        return "node"

    def getEdges(self):
        return iter(self._edges)


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
        self._nodes = {}
        self._edges = {}
        self._neighbors = {}  # cached adjacency
        self._maxuid = 0

    def _new_uid(self):
        self._maxuid += 1
        return self._maxuid

    def add_node(self, obj):
        uid = self._new_uid()
        self._nodes[uid] = obj
        self._neighbors[uid] = set()
        return uid

    def add_edge(self, uid1, uid2):
        uid = self._new_uid()
        # BAD: custom choosen data struct
        #   => may discard _edges alltogether and keep only _neighbors
        self._edges[uid] = (uid1, uid2)
        # BAD: directly saves neighbor uid instead of intermediate 'edge'
        #   => must save ptrs to 'edge' to be able to be replaced by 'graph'
        # BAD: how to connect each neighbor with its own point in GraphNode ?
        #   => only GraphNode may decide it depending on who edge points to
        #     -> [_] DEV pass neighbor node directly into add_edge() of GraphNode
        #   => THINK: passing edge is not exclusive -> you still need _neighbors
        #     when replacing SimpleNode by GraphNode and back to re-supply whole
        #     list of _neighbors to constructor
        #     ALSO you need full-fledged object of Edge -- to remain connected
        #     on one side (surrounding graph knowledge) when replacing edges
        #     connect-point to another node
        self._neighbors[uid1].add(uid2)
        self._neighbors[uid2].add(uid1)
        # NEED: cache added node in adjacency set of each node
        return uid

    def __str__(self):
        return "graph"

    # Prefer nodes for default operations
    def __iter__(self):
        return iter(self._nodes)

    def __getitem__(self, uid):
        return self._nodes[uid]

    def __contains__(self, uid):
        return uid in self._nodes


# API accepts id (instead of objects) and returns objects itself
class IdGraph(BaseGraph):
    pass


# API accepts/returns proxy instance of accessors to IdGraph
#   * each proxy aggregates _uid and _g to be able to operate on itself
#   == weak_ptr == actual object inside Graph may be destroyed any time
class ProxyGraph(object):
    def __init__(self):
        self._g = IdGraph()

    def get_node(self):
        return INode(self._g)


# Wrapper classes extend base container by shortcuts
#   E.G. add_node_connected_to(node, [neighbour_nodes])
#     == add_node() + add_edges_from()
class RootedGraph(ProxyGraph):
    def __init__(self, rootid=None, **kw):
        self._rootid = rootid
        super().__init__(**kw)

    def getRoot(self):
        """ Construct node object with backref to graph """
        return self.get_node(self.rootid)
