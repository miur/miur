import miur.core.fs as fs


class NodeGetParent:
    def process(self, msg):
        path = msg['args'][0]
        p = fs.parent_node(path)
        # _log.info("Change path: {}".format(p))

        # THINK: instead of directly ret msg you can save it to self._msg and
        #   later process this object in send queue
        return {'id': msg['id'], 'rsp': p}


class NodeGetChild:
    def process(self, msg):
        path, entry = msg['args']
        p = fs.child_node(path, entry)
        return {'id': msg['id'], 'rsp': p}


class ListNode:
    def process(self, msg):
        path = msg['args'][0]
        l = fs.list_nodes(path)
        return {'id': msg['id'], 'rsp': l}
