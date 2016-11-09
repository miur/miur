from miur.cursor import state, graph


class Dispatcher:
    """Apply actions to any unrelated global states"""

    def _err_wrong_cmd(self):
        # Move err processing to 'update.py' (make more symmetrical)
        # _log.error("Wrong cmd: {}".format(cmd))
        raise

    def focus_node_next(self):
        if state.cursor is not None and state.entries is not None:
            state.cursor = min(state.cursor + 1, len(state.entries) - 1)

    def focus_node_prev(self):
        if state.cursor is not None and state.entries is not None:
            state.cursor = max(state.cursor - 1, 0)

    def focus_node_beg(self):
        if state.entries is not None:
            state.cursor = 0

    def focus_node_end(self):
        if state.entries is not None:
            state.cursor = len(state.entries) - 1

    def shift_node_parent(self):
        # DEV: combine these multiple queue in single request to *core*
        # state.path =
        graph.parent_node(state.path)  # TEMP: apply directly to global state
        state.entries = graph.list_nodes(state.path)
        state.cursor = 0 if state.entries else None

    def shift_node_current(self):
        if state.cursor is None or state.entries is None:
            return
        state.path = graph.child_node(state.path, state.entries[state.cursor])
        state.entries = graph.list_nodes(state.path)
        state.cursor = 0 if state.entries else None
