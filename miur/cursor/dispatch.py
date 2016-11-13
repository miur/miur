from miur.cursor import state, update, message as msg


class Dispatcher:
    """Apply actions to any unrelated global states"""

    def _err_wrong_cmd(self):
        # Move err processing to 'update.py' (make more symmetrical)
        # _log.error("Wrong cmd: {}".format(cmd))
        raise NotImplementedError

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
        # TEMP: apply directly to global state
        # TEMP: send msg and wait until fully processed (send-recv-apply)
        update.handle(msg.NodeGetParentMsg())
        update.handle(msg.ListNodeMsg())
        state.cursor = 0 if state.entries else None

    def shift_node_current(self):
        if state.cursor is None or state.entries is None:
            return
        # WARN: must send both (p, e) for *core*
        #   => to check if (p, e) is still available in fs
        update.handle(msg.NodeGetChildMsg())
        update.handle(msg.ListNodeMsg())
        state.cursor = 0 if state.entries else None
