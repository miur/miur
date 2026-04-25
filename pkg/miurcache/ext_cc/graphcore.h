#ifndef GRAPHCORE_H
#define GRAPHCORE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  const uint8_t *data;
  size_t size;
} gc_buf;

/* Node record: opaque offset into buffer */
typedef uint32_t gc_node_id;

/* Edge record */
typedef struct {
  gc_node_id dst;
  uint32_t weight;
} gc_edge;

/* Graph handle (no ownership by default) */
typedef struct {
  gc_buf buf;
} gc_graph;

/* ---- Core API ---- */

/* Initialize graph from raw memory */
int gc_graph_init(gc_graph *g, const void *data, size_t size);

/* Validate structure (cheap structural check) */
int gc_graph_validate(const gc_graph *g);

/* Node count */
size_t gc_node_count(const gc_graph *g);

/* Edge iteration */
typedef struct {
  const gc_graph *g;
  size_t pos;
  size_t end;
} gc_edge_iter;

int gc_edge_iter_begin(const gc_graph *g, gc_node_id node, gc_edge_iter *it);
int gc_edge_iter_next(gc_edge_iter *it, gc_edge *out);

/* Random access */
int gc_node_degree(const gc_graph *g, gc_node_id node, size_t *deg);

/* Raw slice (zero-copy exposure) */
int gc_node_edges_raw(const gc_graph *g, gc_node_id node, const uint8_t **ptr,
                      size_t *len);

#ifdef __cplusplus
}
#endif

#endif
