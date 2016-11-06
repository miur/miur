from miur.graph import graph, cursor


# from miur.relay import client
# tcp, unix, mem_share, etc
# coupling = None
# if coupling == 'tcp':
# ALT: self.g.entries = client.send(saddr, self.c.path)
# Server addr
# saddr = None

class Dispatcher:
    """Apply actions to any unrelated global states"""

    def __init__(self):
        self.g = graph
        self.c = cursor
        self.g.entries = self.g.list_nodes(self.c.path)

    def _err_wrong_cmd(self):
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
        self.c.path = self.g.parent_node(self.c.path)
        self.g.entries = self.g.list_nodes(self.c.path)
        self.c.cursor = 0 if self.g.entries else None

    def shift_node_current(self):
        if self.c.cursor is None or self.g.entries is None:
            return
        self.c.path = self.g.child_node(self.c.path, self.g.entries[self.c.cursor])
        self.g.entries = self.g.list_nodes(self.c.path)
        self.c.cursor = 0 if self.g.entries else None
