from miur.share.ifc import ILink

__all__ = ['Deanonymize', 'Anonymize']


class Deanonymize(ILink):
    def __init__(self, factory):
        self._factory = factory

    def __call__(self, pair):
        uid, cmd = pair
        car = self._factory(cmd=cmd, uid=uid)
        self._sink(car)


class Anonymize(ILink):
    def __call__(self, car):
        # _log.info('Response({:x}): {!r}'.format(car.uid, car.rsp))
        self._sink((car.uid, car.rsp))
