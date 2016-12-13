# USAGE: Channel:
# from miur.share.chain import Chain
# from miur.share.osi import *
# self.chain_recv = Chain([Fasten, Deserialize, Desegmentate, _receive], iterator=reversed)
# self.chain_send = Chain([Unfaste, Serialize, Segmentate, _send])

import threading


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
class Chain:
    def __init__(self, functors, *, iterator=lambda x: x):
        self._impl = list(functors)
        self._lock = threading.Lock()
        self._iter = iterator

    @with_lock
    def __call__(self, arg):
        for f in self._iter(self._impl):
            arg = f(arg)
        return arg

    # @with_lock
    # def __getitem__(self, k):
    #     return self._impl.__getitem__(k)
