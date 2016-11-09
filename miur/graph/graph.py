import miur.access.fs as fs  # TEMP:CHG
from miur.relay import aux
from miur.graph import command as cmd

# TEMP:CHG: use intermediate caching
graph = fs
entries = None


def parent_node(p):
    # DEV: rpc 'parent_node' and update 'path' when done
    # TEMP: send msg and wait until fully processed (send-recv-apply)
    aux.put_cmd_threadsafe(cmd.NodeGetParent(p))
    # return graph.parent_node(p)


def child_node(p, e):
    # WARN: must send both (p, e) for *core*
    #   => to check if (p, e) is still available in fs
    # aux.put_cmd_threadsafe(('get_child_node', p, e))
    return graph.child_node(p, e)


def list_nodes(p):
    # aux.put_cmd_threadsafe(('list_node', p))
    return graph.list_nodes(p)
