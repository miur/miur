# NOTE: global neighbors dict is useful only to save memory in sparse graphs
# with for many isolated nodes
#   self._neighbors = defaultdict(set)
#   BET:ALT:USE:(C++): multimap for neighbors (many identical keys)


def run(argv):
    GraphContainer()


# THINK: does NodeContainer needs to know its own uid ?
#   MAYBE: it's enough to pass its own ouid only to methods which require ?
class NodeContainer(object):
    def __init__(self, entity):
        # NOTE: entity must be passed to strategy
        self._entity = entity
        self._neighbors = set()
        # NOTE: multiple strategies per single node possible
        #   BUT: they are composable and can be combined in single strategem
        self._strategy = None
        # NOTE: transformation must be parameterizing the strategy
        self._transform = None

    def __call__(self, g):
        return self._entity

    # NOTE: node may directly gen edges instead of using underlying container
    # NOTE: pass 'g' only on query instead of storing it inside instance
    # BAD: gen node can't provide uids from this function ?
    #   BUT: it has access to 'g' ..., so it can insert new nodes directly
    def neighbors(self, g):
        return self._neighbors

    # ERR: raise error if operation is unsupported for such NodeContainer
    # IMPL:ALT: link_node(node) -- to trigger mutually events on link
    #   * establish duplex connection between nodes
    #   * notify strategy of each other about new established connection
    #   * react to connection by refresh, etc
    # DECIDE: if e_uid node is notified about others remembering its own uid
    def add_neighbor(self, g, uid):
        if uid in self._neighbors:
            raise LookupError(uid)
        self._neighbors.add(uid)

    # NOTE: use any callable object
    # CHG: arguable signature
    # THINK: sep events/strategies for node-located functions and graph-wide
    #   ~ call _strategy directly in each api of NodeContainer
    #   ~ otherwise call on_event() only from GraphContainer
    #   BUT: on_event() is required anyway to process "focus/hover" events, etc
    def on_event(self, g, event):
        return self._strategy(g, event, self._entity)


class GraphContainer(object):
    def __init__(self):
        self.clear()

    def clear(self):
        self._nodes = {}

    def __getitem__(self, uid):
        return self._nodes[uid](self)

    def __setitem__(self, uid, obj):
        self._nodes[uid] = NodeContainer(obj)

    def neighbors(self, uid):
        return self._nodes[uid].neighbors(self)

    def add_arrow(self, b_uid, e_uid):
        if e_uid not in self._nodes:
            raise KeyError(e_uid)
        self._nodes[b_uid].add_neighbor(self, e_uid)
