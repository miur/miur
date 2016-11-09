import time
import logging
import asyncio

from miur.graph import graph, cursor

_log = logging.getLogger(__name__)
qout = asyncio.Queue()
dwait = {}
counter = 0


class NodeGetParentCore:
    def process(self, msg):
        path = msg['args'][0]
        p = graph.parent_node(path)
        # _log.info("New path: {}".format(p))
        return {'id': msg['id'], 'rsp': p}
        # send(graph.parent_node(msg))


# MAYBE: incapsulate even getters from globals to create objects w/o arguments at all ?
#   OR: at least use it as default args
class NodeGetParent:
    def __init__(self, path=None):
        if path is None:
            path = cursor.path
        self._msg = {'cmd': type(self).__name__, 'args': [path]}
        global counter
        counter += 1
        # ALT: directly use hash from dict()
        # BUT: no guarantee it won't change between transactions
        self._msg['id'] = hash((counter, time.clock()))

    def msg(self):
        m = self._msg
        _log.info("Req msg: {}".format(m))
        return m

    def rsp(self, rsp):
        # Use more appropriate type for Path. BUT how to serialize in C ?
        p = rsp
        if not isinstance(p, str):
            raise
        # NEED: incapsulate global state changes
        #   -- another msg bus to eliminate locks and keep definitive order ?
        _log.info("Response: {}".format(p))
        cursor.path = p


class Dispatcher:
    """Apply actions to any unrelated global states"""

    def __init__(self):
        self.g = graph
        self.c = cursor

    def _err_wrong_cmd(self):
        # Move err processing to 'update.py' (make more symmetrical)
        # _log.error("Wrong cmd: {}".format(cmd))
        raise

    def focus_node_next(self):
        if self.c.cursor is not None and self.g.entries is not None:
            self.c.cursor = min(self.c.cursor + 1, len(self.g.entries) - 1)

    def focus_node_prev(self):
        if self.c.cursor is not None and self.g.entries is not None:
            self.c.cursor = max(self.c.cursor - 1, 0)

    def focus_node_beg(self):
        if self.g.entries is not None:
            self.c.cursor = 0

    def focus_node_end(self):
        if self.g.entries is not None:
            self.c.cursor = len(self.g.entries) - 1

    def shift_node_parent(self):
        # Proto
        qout.put_nowait(NodeGetParent())
        c = qout.get_nowait()
        m = c.msg()
        dwait[m['id']] = c
        # NEED: await send(c.msg()), rsp = await recv()
        rsp = NodeGetParentCore().process(m)  # TEMP
        h = rsp['id']
        # EXPL: if recv() =~ qwait.items()
        dwait[h].rsp(rsp['rsp'])
        # USE: remove only on demand
        del dwait[h]

        # DEV: combine these multiple queue in single request to *core*
        # self.c.path = self.g.parent_node(self.c.path)
        self.g.entries = self.g.list_nodes(self.c.path)
        self.c.cursor = 0 if self.g.entries else None

    def shift_node_current(self):
        if self.c.cursor is None or self.g.entries is None:
            return
        self.c.path = self.g.child_node(self.c.path, self.g.entries[self.c.cursor])
        self.g.entries = self.g.list_nodes(self.c.path)
        self.c.cursor = 0 if self.g.entries else None
