import miur.core.fs as fs


class NodeGetParentCore:
    def process(self, msg):
        path = msg['args'][0]
        p = fs.parent_node(path)
        # _log.info("Change path: {}".format(p))

        # THINK: instead of directly ret msg you can save it to self._msg and
        #   later process this object in send queue
        return {'id': msg['id'], 'rsp': p}
