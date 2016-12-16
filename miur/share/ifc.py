__all__ = ['ILink']


# NOTE: return passed obj for chaining : self.bind(obj1).bind(obj2)
# WARN:(link.__call__) impossible to reuse _sink like 'chain.bind(self._sink)'
#   ++ ALSO I can use simple functions as _sink
# Unidirectional
# ALT:(name): ISimplex
class ILink:
    def bind(self, link):
        """Bind next link to this"""
        self._sink = link
        return link

    # THINK? better order when exception
    def plug(self, channel):
        """Mutually interconnect links"""
        channel.bind(self)
        return self.bind(channel)

    def __call__(self, *args, **kwargs):
        """Push processed data into next link"""
        # ALT:FIND: directly assign self.__call__ = self._sink
        self._sink(*args, **kwargs)

    def _sink(self, *args, **kwargs):
        """Private ref to next link"""
        raise NotImplementedError


# USE: impl abstract ifc based on 'abc' OR look at more formalized way of 'zope'
#   import abc
#   __metaclass__ = abc.ABCMeta
#   @abc.abstractmethod
# SEE
#   https://habrahabr.ru/post/72757/
#   https://zopetoolkit.readthedocs.io/en/latest/
#   https://www.python.org/dev/peps/pep-3119/
#   http://javascriptissexy.com/beautiful-javascript-easily-create-chainable-cascading-methods-for-expressiveness/
