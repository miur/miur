from ..base import BaseCommand
import miur.core.fs as fs

__all__ = [
    'NodeGetParentCmd',
    'NodeGetChildCmd',
    'ListNodeCmd',
]


class NodeGetParentCmd(BaseCommand):
    cmd = 'get.node.parent'

    def __init__(self, path):
        self.path = path

    def execute(self):
        # _log.info("Change path: {}".format(self.path))
        return fs.parent_node(self.path)


class NodeGetChildCmd(BaseCommand):
    cmd = 'get.node.child'

    def __init__(self, path, entry):
        self.path = path
        self.entry = entry

    def execute(self):
        return fs.child_node(self.path, self.entry)


class ListNodeCmd(BaseCommand):
    cmd = 'list.node'

    def __init__(self, path):
        self.path = path

    def execute(self):
        return fs.list_nodes(self.path)
