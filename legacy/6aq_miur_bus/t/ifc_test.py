#
# SPDX-FileCopyrightText: 2016 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
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
# NOTE: 'assert ...' skipped when <= '__debug__ == False' <= 'python -O'


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
        a = makeBoundary()
        assert is_func_eq(ifc.Boundary._inner, ifc._undefined)
        assert is_func_eq(a._inner, ifc._undefined)
        assert is_func_eq(a._outer, ifc._undefined)

    def test_bind_a_b(self):
        a, b = makeBoundary(), makeBoundary()
        a.bind(b)
        assert is_func_eq(a._inner, b._outer)

    def test_bind_a_f(self):
        a, f = makeBoundary(), id
        a.bind(f)
        assert a._outer is f
        with pytest.raises(AssertionError):
            a.bind(1)


class TestPlugSlot:
    def test_p_s(self):
        p = ifc.Plug()
        s = ifc.Slot()
        s.bind(p)
        assert p.isbound(s)
        assert s.isbound(p)
        s.unbind(p)
        assert not p.isbound()
        assert not s.isbound()
        # Reverse bind
        p.bind(s)
        assert p.isbound(s)
        assert s.isbound(p)
        # Double bind
        p.bind(s)
        assert p.isbound(s)
        assert s.isbound(p)
        # Reverse unbind
        p.unbind(s)
        assert not p.isbound()
        assert not s.isbound()
        # Double unbind (alt syntax)
        p.unbind(s)
        assert p.isbound(False)
        assert s.isbound(False)


# NEED: mock to check __call__ redirection
class TestLink:
    def test_l(self):
        m = ifc.Link()
        n = ifc.Link()
        n.bind(src=m)
        assert m.isbound(plug=False, slot=n.plug)
        assert n.isbound(plug=m.slot, slot=False)
        n.unbind(src=m)
        assert not m.isbound()
        assert not n.isbound()
        # Reverse bind
        m.bind(dst=n)
        assert m.isbound(plug=False, slot=n.plug)
        assert n.isbound(plug=m.slot, slot=False)
        # Reverse unbind (auto)
        m.unbind()
        assert not m.isbound()
        assert not n.isbound()


# NEED: mock to check callback traversing in chain
class TestChain:
    def test_chain(self):
        b = ifc.Link()
        e = ifc.Link()
        c = ifc.Chain()
        assert not c.isbound()

        c.bind(src=b, dst=e)
        assert c.isbound(plug=b.slot, slot=e.plug)
        c.unbind()
        assert not c.isbound()

        c.bind(src=b, dst=e)
        c.chain = [ifc.Link()]
        assert c.isbound(plug=b.slot, slot=e.plug)
