from miur.share import ifc

__all__ = ['Deanonymize', 'Anonymize']


class Deanonymize(ifc.Link):
    def __init__(self, factory):
        self._factory = factory

    def __call__(self, pair):
        uid, cmd = pair
        car = self._factory(cmd=cmd, uid=uid)
        self._slot(car)


class Anonymize(ifc.Link):
    def __call__(self, car):
        # _log.info('Response({:x}): {!r}'.format(car.uid, car.rsp))
        self._slot((car.uid, car.rsp))
