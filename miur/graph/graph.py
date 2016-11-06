import miur.access.fs as fs  # TEMP:CHG
import miur.graph.cursor as cursor

# TEMP:CHG: use intermediate caching
graph = fs

entries = fs.list_nodes(cursor.path)


def parent_node(p):
    # DEV: rpc 'parent_node' and update 'path' when done
    return graph.parent_node(p)


def child_node(p, e):
    return graph.child_node(p, e)


def list_nodes(p):
    return graph.list_nodes(p)
