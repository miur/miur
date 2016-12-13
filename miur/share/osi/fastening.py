__all__ = ['Fasten', 'Unfasten']


class Fasten:
    def __init__(self, factory):
        self._factory = factory

    def __call__(self, pair):
        uid, cmd = pair
        car = self._factory(cmd=cmd, uid=uid)
        return car


class Unfasten:
    def __call__(self, car):
        return (car.uid, car.cmd)
