from .basechainlink import BaseChainLink

__all__ = ['Anonymize', 'Deanonymize']


class Deanonymize(BaseChainLink):
    def __init__(self, factory):
        self._factory = factory

    def __call__(self, pair):
        uid, cmd = pair
        car = self._factory(cmd=cmd, uid=uid)
        self.sink(car)


class Anonymize(BaseChainLink):
    def __call__(self, car):
        self.sink((car.uid, car.rsp))
