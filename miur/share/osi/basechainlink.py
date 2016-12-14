__all__ = ['BaseChainLink']


class BaseChainLink:
    def __call__(self, arg):
        raise NotImplementedError

    def sink(self, arg):
        raise NotImplementedError
