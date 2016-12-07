# Cursor == path (str)

import os

# IDEA: Access to accessors must be sync (blocking), and thread pool only at core.
# - BAD each long op will block any next short ones
# - for flexibility it would be better for accessors to accept cmds as input instead of callbacks.
#   => then I can reuse all other bus facilities
# - what if remote uses only accessor and core is placed at host?
#   => then thread pool at remote will provide nice efficiency
#   ! in such case I need server attached to accessor anyways !


# Can't be specified for general non-tree graphs
# There you must track your whole path and move back to last adjacent node
def parent_node(p):
    return os.path.dirname(p)


def child_node(p, e):
    return os.path.join(p, e)


def list_nodes(p):
    if os.path.isdir(p):
        # SEE:ALT:(scandir) faster
        # THINK: how to update *dom* by iterator instead of pre-generated list
        return list(sorted(os.listdir(p)))
