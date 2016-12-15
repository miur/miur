import logging

from . import state

_log = logging.getLogger(__name__.split('.', 2)[1])

# BETTER: don't bind 'cmd' to class name (type(self).__name__)
#   -- so you could freely rename classes by adding sfx/prf
# FIXME:TEMP: mix concepts of 'catch_imm_ctx' with 'create_msg'
# DEV: rpc 'parent_node' and update 'path' when done
# TODO:DFL: self.args = args or state.path
# NOTE: __class__.cmd can become unsync
#   ! but we can't use classes directly -- as diff mods can be in diff langs


class BaseMessage:
    cmd = 'base.message'

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


class NodeGetParentMsg(BaseMessage):
    cmd = 'get.node.parent'

    def __init__(self, *args):
        self.path = state.path
        super().__init__(self.path)


class NodeGetChildMsg(BaseMessage):
    cmd = 'get.node.child'

    def __init__(self, *args):
        self.path = state.path
        self.curr = state.entries[state.cursor]
        super().__init__(self.path, self.curr)


class ListNodeMsg(BaseMessage):
    cmd = 'list.node'

    def __init__(self, *args):
        self.path = state.path
        super().__init__(self.path)


class QuitMsg(BaseMessage):
    cmd = 'quit-all'

    def __init__(self, *args):
        self.path = state.path
        super().__init__(self.path)
