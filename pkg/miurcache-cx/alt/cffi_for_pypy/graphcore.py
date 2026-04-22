# WARN: Call overhead (provable)
# Each call: lib.gc_edge_iter_next(...) → crosses Python ↔ C boundary
# Cost: ~50–150 ns per call (CPython dependent)
# CPython version: iterator runs entirely in C loop
# cffi version: loop runs in Python


# graphcore.py
from cffi import FFI

ffi = FFI()

ffi.cdef("""
typedef unsigned int uint32_t;
typedef uint32_t gc_node_id;

typedef struct {
    const unsigned char *data;
    size_t size;
} gc_buf;

typedef struct {
    gc_buf buf;
} gc_graph;

typedef struct {
    gc_node_id dst;
    uint32_t weight;
} gc_edge;

typedef struct {
    const gc_graph *g;
    size_t pos;
    size_t end;
} gc_edge_iter;

int gc_graph_init(gc_graph *g, const void *data, size_t size);
int gc_graph_validate(const gc_graph *g);
size_t gc_node_count(const gc_graph *g);

int gc_node_degree(const gc_graph *g, gc_node_id node, size_t *deg);

int gc_edge_iter_begin(const gc_graph *g, gc_node_id node, gc_edge_iter *it);
int gc_edge_iter_next(gc_edge_iter *it, gc_edge *out);
""")

# load your compiled C library (.so)
lib = ffi.dlopen("libgraphcore.so")


class Graph:
    __slots__ = ("_buf", "_g")

    def __init__(self, buf: bytes | memoryview):
        self._buf = memoryview(buf)  # keep alive

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
