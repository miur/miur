

class NodeProxy(object):
    def __init__(self, g, uid):
        self._g = g
        self._uid = uid

    def __call__(self):
        return self._g[self._uid]

    def __iter__(self):
        for uid in self._g.neighbors(self._uid):
            yield NodeProxy(self._g, uid)
