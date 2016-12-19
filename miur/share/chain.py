import threading
from collections import Sequence

from miur.share import ifc

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
class Chain(Sequence, ifc.Link):
    def __init__(self, links=None, *, src=None, dst=None):
        super().__init__()
        self._lock = threading.Lock()
        self._chain = []
        self[:] = links if links is not None else []
        self.bind(src=src, dst=dst)

    @with_lock
    def bind(self, src=None, dst=None):
        super().bind(src=src, dst=dst)
        self._invalidate()

    @with_lock
    def unbind(self, src=None, dst=None):
        super().unbind(src=src, dst=dst)
        self._invalidate()

    @with_lock
    def __setitem__(self, k, v):
        self._chain.__setitem__(k, v)
        [l.bind(s) for l, s in zip(self._chain, self._chain[1:])]
        self._invalidate()
        _log.debug('Chain: {!r}'.format(self.__call__))

    def _invalidate(self):
        if self._chain:
            self.plug.set_inner(self._chain[0].plug)
            # BAD: we can't mutually unbind() last slot -- it will disconnect Chain.slot from sink
            # ALT: use def __call__(...): self.slot(...) BUT assert __call__ will be failed
            # THINK:USE self.slot.set_inner() or allow functors generally (remove asserts on isinstance)
            self._chain[-1].slot.bind(self.slot, mutual=False)
        else:
            self.plug.set_inner(self._slot)

    def __getitem__(self, k):
        return self._chain[k]

    def __len__(self):
        return len(self._chain)
