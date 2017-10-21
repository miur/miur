from . import entity


def run(argv):
    g = ImmediateGraph(entity.DirEntity('/etc/asciidoc'))
    for n in g.get_root():
        print(n().name)
        for nn in n:
            print('  ' + nn().name)


class ImmediateNode(object):
    def __init__(self, obj):
        self._obj = obj

    def __call__(self):
        return self._obj

    def __iter__(self):
        try:
            it = iter(self._obj)
        except TypeError:
            return
        else:
            for obj in it:
                yield ImmediateNode(obj)


class ImmediateGraph(object):
    def __init__(self, rootobj):
        self._rootobj = rootobj

    def get_root(self):
        return ImmediateNode(self._rootobj)
