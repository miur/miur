import logging
import threading

_log = logging.getLogger(__name__)


# NOTE: mixing ClientProtocol with static vars is BAD idea
# BETTER: re-impl loop.create_server(...) for deterministic order of accept/receive/lost
#   => then parallelism will be controllable and broken lock may become unnecessary
#   BUT: it's currently difficult -- too much to reimplement OR too long to reduce complexity
# USAGE: all_conn = ClientConnections(sid_grp='tcp')

class ClientConnections(dict):
    def __init__(self, sid_grp=None):
        self._lock = threading.Lock()
        self.sid_grp = sid_grp

    def __getitem__(self, cid):
        # THINK? items have refcount so calling their methods is threadsafe after getitem?
        with self._lock:
            return super().__getitem__(cid)

    def __setitem__(self, cid, obj):
        with self._lock:
            return super().__setitem__(cid, obj)

    def __delitem__(self, cid):
        # BAD: crash if waits on _lock until disconnectAll() exits
        with self._lock:
            _log.info('Client {!r} removed'.format(cid))
            return super().__delitem__(cid)

    # ALT: overload generator :: [c.close() for c in conn.values()]
    def disconnectAll(self, sid_grp=None):
        if sid_grp is None:
            sid_grp = self.sid_grp
        # BAD: won't close any connection pending on _lock to in __setitem__
        with self._lock:
            for dst, obj in self.items():
                sid, cid = dst
                if sid == sid_grp:
                    _log.info('Closing the client {!r}'.format(cid))
                    obj.close()

    def ignoreRecvAll(self, sid_grp=None):
        if sid_grp is None:
            sid_grp = self.sid_grp
        # BAD: won't close any connection pending on _lock to in __setitem__
        with self._lock:
            for dst, obj in self.items():
                sid, cid = dst
                if sid == sid_grp:
                    obj.ignore_recv_end()
