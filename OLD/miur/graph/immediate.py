#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
from . import entity, proxy


def run(argv):
    g = ImmediateGraph(entity.DirEntity('/etc/asciidoc'))
    proxy.print2lvl(g.get_root())


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
