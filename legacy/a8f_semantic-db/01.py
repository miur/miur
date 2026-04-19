#!/usr/bin/env python3
#
# SPDX-FileCopyrightText: 2020 Amerlyq <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
"""
Filesystem as semantic graph
"""

import sys
import os, os.path as fs

from pathlib import Path
from typing import Iterator

from rdflib import Graph, Namespace, URIRef, BNode, Literal
from rdflib.namespace import FOAF, RDF, XSD
from rdflib.term import bind


FS = Namespace('urn:fm:fs:')
bind(FS.path, Path)


def scan_paths_under(paths:Iterator[Path]) -> Iterator[Path]:
    """ Returns all found files and directories """
    def _traverse(xs):
        for x in xs:
            yield x
            if x.is_dir() and not x.is_symlink():
                yield from _traverse(x.iterdir())
    yield from _traverse(paths)


class MiurFM(object):
    def __init__(self):
        # FUTURE: actually I need "GraphProxy" with rdf-API
        #   * thin caching layer with <entry>.invalidate() method
        #     MAYBE use rdflib as-is
        #   * auto-querying: items not found in cache are sync/async/gen fetched
        #     CHECK maybe rdflib already has proxying (remote-query) mechanics
        g = Graph()
        g.bind('fs', FS)
        self._g = g

    def load_dirs(self, dirs):
        g = self._g
        for p in scan_paths_under(dirs):
            xp = BNode(str(p.parent))
            x = g.resource(BNode(str(p)))
            x.add(FS.path, Literal(p))
            x.add(FS.name, Literal(p.name))
            x.add(FS.parent, xp)
            g.add((xp, FS.child, x.identifier))
        # HACK:(replace): root.parent == root
        xroot = BNode(str(dirs[0]))
        g.set((xroot, FS.parent, xroot))


    def print_db(self):
        # VIZ: n3 turtle ntriples pretty-xml json-ld
        print(self._g.serialize(format='ntriples').decode('utf-8'))

    def print_at(self, cwd):
        d = self._g.resource(BNode(str(cwd)))
        for x in d.objects(FS.child):
            print(x.value(FS.name))

    # BAD:ARCH: dirty hack to not keep widget explicit state for picking items
    def select_at(self, cwd, *, sortkey=None):
        d = self._g.resource(BNode(str(cwd)))

        xs = d.objects(FS.child)
        if sortkey is not None:
            xs = sorted(xs, key=sortkey)
        xs = list(xs)
        nms = [x.value(FS.name).value for x in xs]

        assert d.value(FS.parent)
        xs.insert(0, d.value(FS.parent))
        nms.insert(0, '..')

        nums = [sum(map(bool, x.objects(FS.child))) for x in xs]

        i = yield ((i, nm, nums[i]) for i, nm in enumerate(nms))
        if not isinstance(i, int):
            # OR? sel = d.value(FS.child / FS.name, i)
            i = nms.index(i)
        return xs[i].value(FS.path)


def uix_cli(fm, cwd):
    by_name = lambda x: x.value(FS.name).value
    try:
        while True:
            try:
                loop = fm.select_at(cwd, sortkey=by_name)
                print(f'\n[{cwd}]')
                for i, k, n in next(loop):
                    print(f'{i:>2d}) {k:<16} {{{n}}}')
                r = input('=> ')
                loop.send(int(r) if r.isdigit() else r)
            except (IndexError, KeyError, ValueError) as ex:
                print(f'ERROR: {ex}', file=sys.stderr)
            except StopIteration as ex:
                cwd = ex.value
    except EOFError:
        pass


def main(exepath):
    mtop = sys.modules[__name__]
    here, __appname__ = fs.split(exepath)

    d_root = Path.home() / '.Mathematica'
    # d_root = Path.home() / 'j8'
    fm = MiurFM()
    fm.load_dirs([d_root])
    # fm.print_db()
    # fm.print_at(d_root)
    uix_cli(fm, d_root)


if __name__ == '__main__':
    main(__file__)
