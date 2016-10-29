# Cursor == path (str)

import os


# Can't be specified for general non-tree graphs
# There you must track your whole path and move back to last adjacent node
def parent_node(p):
    return os.path.dirname(p)


def child_node(p, e):
    return os.path.join(p, e)


def list_nodes(p):
    if os.path.isdir(p):
        return list(sorted(os.listdir(p)))
