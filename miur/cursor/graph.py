import miur.core.fs as fs  # TEMP:MOVE
from miur.ui import client
from miur.cursor import command as cmd


def parent_node(p):
    # DEV: rpc 'parent_node' and update 'path' when done
    # TEMP: send msg and wait until fully processed (send-recv-apply)
    client.put_cmd_threadsafe(cmd.NodeGetParent(p))
    # return fs.parent_node(p)


def child_node(p, e):
    # WARN: must send both (p, e) for *core*
    #   => to check if (p, e) is still available in fs
    # client.put_cmd_threadsafe(('get_child_node', p, e))
    return fs.child_node(p, e)


def list_nodes(p):
    # client.put_cmd_threadsafe(('list_node', p))
    return fs.list_nodes(p)
