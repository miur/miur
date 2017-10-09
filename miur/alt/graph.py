import logging

_log = logging.getLogger(__name__.split('.', 2)[1])

# https://bradfieldcs.com/algos/graphs/representing-a-graph/
# https://stackoverflow.com/questions/19472530/representing-graphs-data-structure-in-python
#   http://igraph.org/
#   http://networkx.github.io/
#   TRY:USE: https://networkx.github.io/documentation/stable/tutorial.html
#     => use complete graph library to understand necessary API and limitations

# ++++ https://en.wikipedia.org/wiki/Graph_rewriting
# https://en.wikipedia.org/wiki/Graph_(abstract_data_type)#Representations
# ++ https://en.wikipedia.org/wiki/Gremlin_(programming_language)
#   https://en.wikipedia.org/wiki/OrientDB
#   +? http://www.gstore-pku.com/en/


def run(argv):
    g = GraphContainer()
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
class IGraph(INode):
    pass


# MAYBE: container inherited from GraphProvider
# attrs == state
#   + memory (history)
#   + knowledge about other nodes (environment / surrounding world)
# edges == IO monad :: there is no more IO to query info from Node
#   = bus / pipe -- mirror some piece of data from one end to another
#   + modify/convert content when transferring
#   + EdgeGraph -- encapsulates whole modification logic
class GraphContainer(object):
    # nodes + attrs
    # edges + attrs
    def __str__(self):
        return "graph"

    def getRoot(self):
        """ Construct node object with backref to graph """
        return INode(self)


class GraphProvider(IGraph):
    pass
