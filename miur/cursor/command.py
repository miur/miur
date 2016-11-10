import logging

from . import state
from miur.ui import client

_log = logging.getLogger(__name__)

# BETTER: don't bind 'cmd' to class name (type(self).__name__)
#   -- so you could freely rename classes by adding sfx/prf


class Command:
    def __init__(self, *args):
        _log.fatal('Args: {!r}'.format(args))
        if args:
            self.args = args

    def msg(self, args={}):
        m = {'cmd': self.cmd}
        if hasattr(self, 'args'):
            m.update({'args': self.args})
        m.update(args)
        _log.info("Req msg: {}".format(m))
        return m

    def rsp(self, obj):
        _log.info("Response: {!r}".format(obj))
        pass


# DEV: rpc 'parent_node' and update 'path' when done
class NodeGetParent(Command):
    cmd = 'get.node.parent'

    def rsp(self, obj):
        # Use more appropriate type for Path. BUT how to serialize in C ?
        p = obj['rsp']
        super().rsp(p)
        if not isinstance(p, str):
            _log.fatal('Err obj: {!r}'.format(p))
            raise TypeError
        state.path = p


class NodeGetChild(Command):
    cmd = 'get.node.child'
    # TODO:DFL: self.args = args or state.path

    def rsp(self, obj):
        # Use more appropriate type for Path. BUT how to serialize in C ?
        p = obj['rsp']
        super().rsp(p)
        if not isinstance(p, str):
            _log.fatal('Err obj: {!r}'.format(p))
            raise TypeError
        state.path = p


class ListNode(Command):
    cmd = 'list.node'

    def rsp(self, obj):
        l = obj['rsp']
        super().rsp(l)
        if l is None:
            # BAD: stacks path elements even if can't go inside /path/to/file/file/file/...
            #   => MUST undo path change left/right back to curr dir
            return
        if not isinstance(l, list):
            _log.fatal('Err obj: {!r}'.format(l))
            raise TypeError
        state.entries = l


class Quit(Command):
    cmd = 'quit-all'

    def rsp(self, obj):
        c = obj['cmd']
        super().rsp(c)
