import uuid
from subprocess import check_output, CalledProcessError


def execute(cmd):
    try:
        out = check_output(cmd)
    except CalledProcessError:
        return None
    else:
        return out[:-1].decode('utf-8').splitlines()


# NEED:
#   insert items into satellite
#   use satellite info
def list2dom(items, parent=None):
    metainfo = {uuid.uuid4(): e for e in items}
    edges = set(metainfo.keys())
    if parent is not None:
        edges.add(parent)
    node = uuid.uuid4()
    return (node, edges, metainfo)


def cmd2dom(cmd, parent=None):
    return list2dom(execute(cmd), parent)
