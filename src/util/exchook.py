import os
import sys
from contextlib import contextmanager
from types import TracebackType
from typing import Any, Callable, Iterator, Type

from .logger import log


@contextmanager
def enable_warnings(error: bool = True) -> Iterator[None]:
    if sys.warnoptions:
        return

    import warnings

    # DEBUG: ResourceWarning(asyncio), DeprecationWarning(ipython), etc.
    if not error:
        warnings.simplefilter("always")  # OR="default" to print 1st only
        return

    # SRC: https://stackoverflow.com/questions/22373927/get-traceback-of-warnings
    # def warn_with_traceback(message, category, filename, lineno, file=None, line=None):
    #     log = file if hasattr(file,'write') else sys.stderr
    #     traceback.print_stack(file=log)
    #     log.write(warnings.formatwarning(message, category, filename, lineno, line))
    # warnings.showwarning = warn_with_traceback

    warnings.filterwarnings("error")  # Treat warnings as errors
    try:
        yield
    # except Warning:
    #     log.warning(traceback.format_exc())  # print traceback
    finally:
        warnings.resetwarnings()  # Back to default behavior


def exception_handler(
    etype: Type[BaseException],
    value: BaseException,
    tb: TracebackType | None,
) -> Any:
    ## WTF: I already did that in wrapper().finally, why it has no effect ?
    # C.endwin()
    msg = (str(value.args[0]) if value.args else "") + "".join(
        f"{os.linesep}  \\ {k} = {v}" for k, v in vars(value).items()
    )
    value.args = (msg, *value.args[1:])

    _orig_write = log.write
    log.config(write=sys.stderr.write)
    try:
        # pylint:disable=import-outside-toplevel
        import traceback as TR

        # OR: (fr.* for fr in __import__("traceback").extract_tb(tb))
        bt = "".join(TR.format_tb(tb)).rstrip().replace(os.linesep, os.linesep + "\\")
        log.info("Traceback (most recent call last):" + os.linesep + "\\" + bt)
        err = "".join(TR.format_exception_only(etype, value)).rstrip()
        ## DISABLED: it seems they are already appended ?
        # if value.__notes__:
        #     err += "".join(os.linesep + "  \\ " + note for note in value.__notes__)
        log.error(err)
        # _orig_excepthook(etype, value, tb)  # OR: sys.__excepthook__(...)
    finally:
        log.config(write=_orig_write)


@contextmanager
def log_excepthook() -> (
    Iterator[Callable[[Type[BaseException], BaseException, TracebackType], Any]]
):
    _orig_excepthook = sys.excepthook
    try:
        sys.excepthook = exception_handler
        yield exception_handler
    finally:
        sys.excepthook = _orig_excepthook
