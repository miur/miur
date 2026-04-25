from cffi import FFI

ffibuilder = FFI()

# ---- C API definition (same as before) ----
ffibuilder.cdef("""
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

# ---- embed your pure C implementation ----
ffibuilder.set_source(
    "graphcore_cffi",  # module name
    """
    #include "graphcore.h"
    """,
    sources=["graphcore.c"],  # your real implementation
    extra_compile_args=["-O3", "-std=c2x"],
)

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
