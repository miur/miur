import logging

from . import state

_log = logging.getLogger(__name__)

# Use more appropriate type for Path. BUT how to serialize in C ?


def prove_path(p):
    if not isinstance(p, str):
        _log.fatal('Err obj: {!r}'.format(p))
        raise TypeError
    return p


def prove_entries(l):
    if l is not None and not isinstance(l, list):
        _log.fatal('Err obj: {!r}'.format(l))
        raise TypeError
    return l


class BaseEffect:
    cmd = 'base.effect'  # REM: after moving 'BaseEffect' into sep file

    def __init__(self, msg, rsp):
        self.msg = msg
        self.rsp = rsp  # REM
        _log.info("Response: {!r}".format(self.rsp))

    def apply(self):
        _log.info("Apply: {!r}".format(self.rsp))


# DEV: rpc 'parent_node' and update 'path' when done
class NodeGetParentEff(BaseEffect):
    cmd = 'get.node.parent'

    def __init__(self, msg, rsp):
        super().__init__(msg, rsp)
        self.path = prove_path(rsp)

    def apply(self):
        state.path = self.path
        super().apply()


class NodeGetChildEff(BaseEffect):
    cmd = 'get.node.child'

    def __init__(self, msg, rsp):
        super().__init__(msg, rsp)
        self.path = prove_path(rsp)

    def apply(self):
        state.path = self.path
        super().apply()


class ListNodeEff(BaseEffect):
    cmd = 'list.node'

    def __init__(self, msg, rsp):
        super().__init__(msg, rsp)
        self.entries = prove_entries(rsp)

    def apply(self):
        # BAD: stacks path elements even if can't go inside /path/to/file/file/file/...
        #   => MUST undo path change left/right back to curr dir
        if self.entries is not None:
            state.entries = self.entries
        super().apply()


class QuitEff(BaseEffect):
    cmd = 'quit-all'

    def __init__(self, msg, rsp):
        super().__init__(msg, rsp)

    def apply(self):
        super().apply()
