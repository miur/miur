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


# USAGE: anywhere in test_*(): assert bps.parse("") ==
#   @pytest.fixture; def bps(): return BudParser()
#   with pytest.raises(BudSyntaxError): func(2)


class TestBoundary:
    def setup(self):
        class TLink(ifc.Link):
            def __call__(self):
                raise NotImplementedError
        self.link = TLink()

    def test_init(self):
        assert ifc._undefined is ifc._undefined
