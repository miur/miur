import pytest

from miur.share import ifc

# SEE:
# % pytest
#   +++ https://habrahabr.ru/post/269759/
#   https://habrahabr.ru/company/yandex/blog/242795/
#   https://www.isotoma.com/blog/2014/05/01/using-mock-patch-in-automated-unit-testing/
#   http://programeveryday.com/post/pytest-creating-and-using-fixtures-for-streamlined-testing/
#   + http://stackoverflow.com/questions/9757299/python-testing-an-abstract-base-class
# % perf
#   http://stackoverflow.com/questions/1777556/alternatives-to-gprof/1779343#1779343
#   http://stackoverflow.com/questions/4295799/how-to-improve-performance-of-this-code/4299378#4299378
#   http://stackoverflow.com/questions/18736473/optimizing-python-performance-in-function-calls


def concreted(abclass):
    class concreteCls(abclass):
        pass
    concreteCls.__abstractmethods__ = frozenset()
    return type('DummyConcrete' + abclass.__name__, (concreteCls,), {})


def is_func_eq(f, g):
    return f.__code__.co_code == g.__code__.co_code


@pytest.fixture
def makeBoundary():
    cls = concreted(ifc.Boundary)
    b = cls()
    b._outer_t = cls
    return b


def test_undef():
    with pytest.raises(NotImplementedError):
        ifc._undefined()


class TestBoundary:
    def test_initial(self):
        self.a = makeBoundary()
        assert is_func_eq(ifc.Boundary._inner, ifc._undefined)
        assert is_func_eq(a._inner, ifc._undefined)
        assert is_func_eq(a._outer, ifc._undefined)

    def test_bind_ab(self):
        a, b = makeBoundary(), makeBoundary()
        a.bind(b)
        assert a._inner is b._outer
