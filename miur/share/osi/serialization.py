# TODO: generalize for any object type by some 'unfactory()/accessor()' passed to Serialize
# ALT:(name): CommandPresentation

import pickle

from miur.share.ifc import ILink

__all__ = ['Deserialize', 'Serialize']


class Deserialize(ILink):
    def __init__(self, factory):
        self._factory = factory

    def __call__(self, data):
        kv = pickle.loads(data)
        cmd = self._factory(kv['cmd'], *kv['args'])
        self._sink((kv['id'], cmd))


# NOTE: can split single cmd into multiple msgs -- for streaming, etc
# RFC: remove intermediate 'dict' (if possible)
class Serialize(ILink):
    def __call__(self, pair):
        uid, rsp = pair
        obj = {'id': uid, 'rsp': rsp}
        data = pickle.dumps(obj, protocol=pickle.HIGHEST_PROTOCOL)
        self._sink(data)
