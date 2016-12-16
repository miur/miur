__all__ = ['ILink']


# NOTE: return passed obj for chaining : self.bind(obj1).bind(obj2)
# WARN:(link.__call__) impossible to reuse _sink like 'chain.bind(self._sink)'
#   ++ ALSO I can use simple functions as _sink
# Unidirectional
# ALT:(name): ISimplex
class ILink:
    def __new__(cls, *p, **k):
        # CHECK:WTF: when ILink used as mix-in ?
        obj = object.__new__(cls)
        obj._sink = cls.__sink
        return obj

    @staticmethod
    def __sink(*p, **k):
        """Private ref to next link"""
        raise NotImplementedError

    def bind(self, link, *, mutual=False):
        """Bind next link to this"""
        # THINK? better order when exception
        # THINK? where mutual linking may be useful ? Only short-circuit ?
        if mutual:
            link.bind(self)
        self.unbind(link)
        self._sink = link
        return link

    def unbind(self, link, *, mutual=False):
        """Safe unbind to override by dispatchers"""
        if mutual:
            link.unbind(self)
        if self._sink != self.__sink:
            assert self._sink == link
            # CHECK: if this reassign works as expected
            self._sink = self.__sink
        return link

    def __call__(self, *args, **kwargs):
        """Push processed data into next link"""
        # ALT:FIND: directly assign self.__call__ = self._sink
        self._sink(*args, **kwargs)


# USE: impl abstract ifc based on 'abc' OR look at more formalized way of 'zope'
#   import abc
#   __metaclass__ = abc.ABCMeta
#   @abc.abstractmethod
# SEE
#   https://habrahabr.ru/post/72757/
#   https://zopetoolkit.readthedocs.io/en/latest/
#   https://www.python.org/dev/peps/pep-3119/
#   http://javascriptissexy.com/beautiful-javascript-easily-create-chainable-cascading-methods-for-expressiveness/
