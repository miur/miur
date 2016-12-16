import threading
from collections import Sequence

from miur.share.ifc import ILink

import logging
_log = logging.getLogger(__name__.split('.', 2)[1])


# http://stackoverflow.com/questions/13610654/how-to-make-built-in-containers-sets-dicts-lists-thread-safe
def with_lock(method):
    def _with_lock_method(self, *args, **kwargs):
        with self._lock:
            return method(self, *args, **kwargs)
    return _with_lock_method


# TEMP: completely recreate chain to change functors seq
# TODO: destroy Chain from the end to eliminate all partially processed
#   -- generators and force full msgs to buble up
# ALT: instead of generators with yield use manual FSM mapping N calls to M calls
#   => OR pass callback individually to each call BUT? how to be with outer loop ?
#       !! can't be passed carried in args -- as too much nested levels to pass
#           callback to the last chain element
#   * return as fast as all passed args / data blob processed
#   * objects in chain can be paused in intermediate position, waiting on more data
#       BAD: intermediate positions means it's possible only with generators/yield
#           => as we must remember position inside each function
# NOTE: generators are very sweet: we can implement protocol with heartbeat,
#   -- add it to Chain between serializer and segmentor and it will simply work
# ALT:BAD? we integrate segmentor into transport and process inside Chain only single-in-single-out msgs
# ATT: chain N:M is one-way pipe and can't return anything back
#   = 'None' when wholly consumed small packet, accumulated multiple 'car' for long data packet
class Chain(Sequence, ILink):
    def __init__(self, call=None, sink=None, links=None):
        self._lock = threading.Lock()
        self._chain = []
        if call:
            call.bind(self)
        if sink:
            self.bind(sink)
        if links:
            self[:] = links

    @with_lock
    def bind(self, link):
        super().bind(link)
        self._invalidate()
        return link

    def __call__(self, *args, **kw):
        return self._call(*args, **kw)

    @with_lock
    def __setitem__(self, k, v):
        self._chain.__setitem__(k, v)
        [l.bind(s) for l, s in zip(self._chain, self._chain[1:])]
        self._invalidate()
        _log.debug('Chain: {!r}'.format(self.__call__))

    def _invalidate(self):
        if self._chain:
            self._call = self._chain[0]
            self._chain[-1].bind(self._sink)
        else:
            self._call = self._sink

    def __getitem__(self, k):
        return self._chain[k]

    def __len__(self):
        return len(self._chain)
