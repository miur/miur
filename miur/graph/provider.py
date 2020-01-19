import os

from . import graph, entity


class FSTreeProvider(object):
    def __init__(self, path):
        self._path = path

    def __call__(self):
        g = graph.RootedGraphContainer()
        ruid = g.add_object(entity.DirEntity(self._path))
        g.set_root(ruid)
        cache = {self._path: ruid}
        for root, dirs, files in os.walk(self._path):
            # print(root)
            duid = cache[root]

            for name in dirs:
                # print(" [" + os.path.join(root, name) + "]")
                p = os.path.join(root, name)
                uid = g.add_object(entity.DirEntity(p))
                g.add_edge(duid, uid)
                cache[p] = uid

            for name in files:
                # print("  " + os.path.join(root, name))
                p = os.path.join(root, name)
                uid = g.add_object(entity.FileEntity(p))
                g.add_edge(duid, uid)
                cache[p] = uid
        return g
