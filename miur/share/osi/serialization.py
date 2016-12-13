# TODO: generalize for any object type by some 'unfactory()/accessor()' passed to Serialize

import pickle

__all__ = ['Deserialize', 'Serialize']


class Deserialize:
    def __init__(self, factory):
        self._factory = factory

    def __call__(self, data):
        kv = pickle.loads(data)
        cmd = self._factory(kv['cmd'], *kv['args'])
        return (kv['id'], cmd)


class Serialize:
    def __call__(self, pair):
        uid, rsp = pair
        obj = {'id': uid, 'rsp': rsp}
        data = pickle.dumps(obj, protocol=pickle.HIGHEST_PROTOCOL)
        return data
