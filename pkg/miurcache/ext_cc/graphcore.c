#include "graphcore.h"

static inline uint32_t
rd32(uint8_t const* p)
{
    return (uint32_t)p[0] | ((uint32_t)p[1] << 8) | ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}

int
gc_graph_init(gc_graph* g, void const* data, size_t size)
{
    if (!g || !data || size < 4) {
        return -1;
    }
    g->buf.data = (uint8_t const*)data;
    g->buf.size = size;
    return 0;
}

size_t
gc_node_count(gc_graph const* g)
{
    return rd32(g->buf.data);
}

static int
get_offsets(gc_graph const* g, uint8_t const** out)
{
    size_t n = gc_node_count(g);
    size_t need = 4 + (n + 1) * 4;
    if (g->buf.size < need) {
        return -1;
    }
    *out = g->buf.data + 4;
    return 0;
}

int
gc_graph_validate(gc_graph const* g)
{
    uint8_t const* offs;
    if (get_offsets(g, &offs)) {
        return -1;
    }

    size_t n = gc_node_count(g);
    uint32_t prev = 0;

    for (size_t i = 0; i <= n; i++) {
        uint32_t cur = rd32(offs + i * 4);
        if (cur < prev) {
            return -1;
        }
        if (cur > g->buf.size) {
            return -1;
        }
        prev = cur;
    }
    return 0;
}

int
gc_node_degree(gc_graph const* g, gc_node_id node, size_t* deg)
{
    uint8_t const* offs;
    if (get_offsets(g, &offs)) {
        return -1;
    }

    size_t n = gc_node_count(g);
    if (node >= n) {
        return -1;
    }

    uint32_t a = rd32(offs + node * 4);
    uint32_t b = rd32(offs + (node + 1) * 4);

    if (b < a) {
        return -1;
    }
    *deg = (b - a) / 8;
    return 0;
}

int
gc_edge_iter_begin(gc_graph const* g, gc_node_id node, gc_edge_iter* it)
{
    uint8_t const* offs;
    if (get_offsets(g, &offs)) {
        return -1;
    }

    size_t n = gc_node_count(g);
    if (node >= n) {
        return -1;
    }

    it->g = g;
    it->pos = rd32(offs + node * 4);
    it->end = rd32(offs + (node + 1) * 4);
    return 0;
}

int
gc_edge_iter_next(gc_edge_iter* it, gc_edge* out)
{
    if (it->pos >= it->end) {
        return 0;
    }

    uint8_t const* p = it->g->buf.data + it->pos;

    out->dst = rd32(p);
    out->weight = rd32(p + 4);

    it->pos += 8;
    return 1;
}

int
gc_node_edges_raw(gc_graph const* g, gc_node_id node, uint8_t const** ptr, size_t* len)
{
    uint8_t const* offs;
    if (get_offsets(g, &offs)) {
        return -1;
    }

    size_t n = gc_node_count(g);
    if (node >= n) {
        return -1;
    }

    uint32_t a = rd32(offs + node * 4);
    uint32_t b = rd32(offs + (node + 1) * 4);

    *ptr = g->buf.data + a;
    *len = b - a;
    return 0;
}
