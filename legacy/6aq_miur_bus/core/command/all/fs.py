#
# SPDX-FileCopyrightText: 2016 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
from ..base import BaseCommand
import miur.core.fs as fs

__all__ = [
    'NodeGetParentCmd',
    'NodeGetChildCmd',
    'ListNodeCmd',
]


class NodeGetParentCmd(BaseCommand):
    cmd = 'get.node.parent'

    def __init__(self, ctx, path):
        self.path = path

    def execute(self):
        # _log.info("Change path: {}".format(self.path))
        return fs.parent_node(self.path)


class NodeGetChildCmd(BaseCommand):
    cmd = 'get.node.child'

    def __init__(self, ctx, path, entry):
        self.path = path
        self.entry = entry

    def execute(self):
        return fs.child_node(self.path, self.entry)


class ListNodeCmd(BaseCommand):
    cmd = 'list.node'

    def __init__(self, ctx, path):
        self.path = path

    def execute(self):
        return fs.list_nodes(self.path)
