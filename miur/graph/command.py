import logging

from miur.graph import cursor

_log = logging.getLogger(__name__)


# MAYBE: incapsulate even getters from globals to create objects w/o arguments at all ?
#   OR: at least use it as default args
class NodeGetParent:
    def __init__(self, path=None):
        if path is None:
            path = cursor.path
        self._msg = {'cmd': type(self).__name__, 'args': [path]}

    def msg(self):
        m = self._msg
        _log.info("Req msg: {}".format(m))
        return m

    def rsp(self, obj):
        # Use more appropriate type for Path. BUT how to serialize in C ?
        p = obj['rsp']
        if not isinstance(p, str):
            raise
        _log.info("Response: {}".format(p))

        # NEED: incapsulate global state changes
        #   -- another msg bus to eliminate locks and keep definitive order ?
        #   -- return {'cursor.path' = p} for registry hive, and apply changes in another executor
        #   ?? maybe directly send registry hive path with response ??
        #       (-) you need verify path anyways
        #       (-) too tight coupling on sent format
        cursor.path = p


# FIXME:BETTER: don't bind 'cmd' to class name -- so you could freely rename
#   classes by adding sfx/prf
class Quit:
    def msg(self):
        return {'cmd': type(self).__name__}

    def rsp(self, obj):
        pass
