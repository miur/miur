# USAGE:(build/install): First time or after C changes:
#   $ python build_graphcore.py
# This generates: graphcore_cffi.*.so. Then:
#   $ python -c "from graphcore import Graph"
from graphcore_cffi import ffi, lib


class Graph:
    __slots__ = ("_buf", "_g")

    def __init__(self, buf: bytes | memoryview):
        self._buf = memoryview(buf)

        self._g = ffi.new("gc_graph *")

        ptr = ffi.from_buffer(self._buf)

        if lib.gc_graph_init(self._g, ptr, len(self._buf)):
            raise ValueError("init failed")

        if lib.gc_graph_validate(self._g):
            raise ValueError("invalid graph")

    def node_count(self):
        return lib.gc_node_count(self._g)

    def degree(self, node: int):
        deg = ffi.new("size_t *")
        if lib.gc_node_degree(self._g, node, deg):
            raise IndexError
        return deg[0]

    def edges(self, node: int):
        it = ffi.new("gc_edge_iter *")
        if lib.gc_edge_iter_begin(self._g, node, it):
            raise IndexError

        out = []
        e = ffi.new("gc_edge *")

        while lib.gc_edge_iter_next(it, e):
            out.append((e.dst, e.weight))

        return out
