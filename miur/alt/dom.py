import logging
from collections import deque

# NOTE:
#   * main static graph composed from [TopologyNode]

_log = logging.getLogger(__name__.split('.', 2)[1])


def run(argv):
    dom = Dom()
    root_uid = dom.add_node(RealNode())

    attr_uid = dom.add_node(AttributeNode({'name': 'myname'}))
    dom[root_uid].add(attr_uid)

    topo_node = TopologyNode(dom.add_node(None) for i in range(3))
    topo_uid = dom.add_node(topo_node)
    dom[root_uid].add(topo_uid)

    view_node = ViewNode(lambda a: reversed(sorted(a)), topo_node)
    view_uid = dom.add_node(view_node)
    dom[root_uid].add(view_uid)

    print(dom)
    print(dom.nameof(root_uid))


class BaseNode(object):
    pass


# NOTE: used to contain virtual graphs data (w/o converting to *dom* real nodes)
class GraphNode(BaseNode):
    def __init__(self, iterator=None):
        # TRY: rename Dom() -> graph() -- DEV/FIND general graph() container
        self._dom = Dom() if iterator is None else Dom(iterator)


class TopologyNode(BaseNode):
    def __init__(self, iterator=None):
        self._edges = set() if iterator is None else set(iterator)

    def __str__(self):
        return '\n'.join(' * {}'.format(uid) for uid in sorted(self))

    def __iter__(self):
        return iter(self._edges)

    def add(self, uid):
        self._edges.add(uid)


# TEMP: sep type (despite being same as TopologyNode) until I decide about superflat
# NOTE: any RealNode w/o TopologyNode is LeafNode BUT topology can be added when needed
#   !! never access to raw edges of underlying topology -- only to generated view
class RealNode(TopologyNode):
    pass


# NOTE: general node for grouping heterogeneous nodes together
class GroupNode(TopologyNode):
    pass


# NOTE: guaranteed order
#   HACK: superflat with ViewNode for RealNode.multichoice and TopologyNode.edges alike
class ViewNode(BaseNode):
    def __init__(self, transf=None, iterator=None):
        if iterator is None:
            entries = list()
        elif transf is None:
            entries = list(iterator)
        else:
            entries = list(transf(iterator))
        self._entries = entries

    def __iter__(self):
        return iter(self._entries)

    def __str__(self):
        return '\n'.join(' - {}'.format(e) for e in self)


# NOTE:VIZ. visible name, node adding timestamp, etc
#   => dict() may be replaced by GroupNode of multiple AttributeNode per each VAR
class AttributeNode(BaseNode):
    def __init__(self, iterator=None):
        self._attributes = dict() if iterator is None else dict(iterator)

    def __str__(self):
        return '\n'.join('  {}: {}'.format(k, v)
                         for k, v in sorted(self._attributes.items()))

    def __contains__(self, attr):
        return attr in self._attributes

    def __getitem__(self, attr):
        return self._attributes[attr]


# NOTE: group diff variants of same node
class HistoryNode(BaseNode):
    def __init__(self, iterator=None):
        self._history = deque() if iterator is None else deque(iterator)


class GeneratorNode(BaseNode):
    def __init__(self, provider, dst_uid, as_edges):
        self._provider = provider
        self._node

    # NOTE: on access to underlying data => generate them
    def __iter__(self):
        return iter(self._entries)


# NOTE: on access node is replaced by its dedicated type (only once)
#   HACK: lazy -- is partial case of GeneratorNode(once=True)
class LazyNode(GeneratorNode):
    def __init__(self, dst_uid, as_edges):
        super().__init__(self.uid)


# NOTE: afterward convert into distributed subsystem-condensed model
# TRY: split on Dom (encapsulate all unique data) and Graph (impl all ops)
class Dom(object):
    def __init__(self):
        self._nodes = {}
        self._maxuid = 0

    def __str__(self):
        return '\n'.join('{} :: {}\n{}\n'.format(uid, node.__class__.__name__, node)
                         for uid, node in sorted(self._nodes.items()))

    def __iter__(self):
        return iter(self._nodes)

    def __getitem__(self, uid):
        return self._nodes[uid]

    # def new_uid(self):
    #     uid = uuid.uuid4()
    #     while uid in self._nodes:
    #         uid = uuid.uuid4()
    #     return uid

    # NOTE: monotonous counter WARN: guard inc for multithread
    #   ++ can compare node creation order by cmp their uids
    #     * it must be enough for ro- graph
    #     * for rw- graph you also need node-specific 'mtime' beside 'ctime'
    def new_uid(self):
        self._maxuid += 1
        return self._maxuid

    def add_node(self, node):
        uid = self.new_uid()
        self._nodes[uid] = node
        return uid

    def by_typeof(self, uid, type):
        for euid in self[uid]:
            if isinstance(self[euid], AttributeNode):
                return euid

    # FIXME: works only for RealNode
    def propof(self, uid, prop):
        if isinstance(self[uid], RealNode):
            euid = self.by_typeof(uid, AttributeNode)
            if euid and prop in self[euid]:
                return self[euid][prop]

    # NOTE: prop with fallback
    def nameof(self, uid):
        name = self.propof(uid, 'name')
        if not name:
            name = str(uid)
        return name
