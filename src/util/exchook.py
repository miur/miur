import sys
from contextlib import contextmanager

from .logger import log

if globals().get("TYPE_CHECKING"):
    from types import TracebackType
    from typing import Any, Callable, Iterator, Optional, Type


@contextmanager
def enable_warnings(error: bool = True) -> "Iterator[None]":
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
    _etype: "Type[BaseException]",
    value: "BaseException",
    _tb: "Optional[TracebackType]",
) -> "Any":
    # pylint:disable=import-outside-toplevel
    import traceback as TR
    from os import linesep as NL

    # TEMP: verify everything we have is actually printed with backtrace
    log.error("<Exception>")
    if hasattr(value, "__notes__"):
        log.error("+__notes__+")
    if value.__cause__:
        log.error("+__cause__+")

    msg = (str(value.args[0]) if value.args else "") + "".join(
        f"{NL}  \\ {k} = {v}" for k, v in vars(value).items()
    )
    value.args = (msg, *value.args[1:])

    # MAYBE: write unconditionally to tty/stderr
    # _orig_write = log.write
    # log.config(write=g.io.ttyout.write)
    try:
        log.error("".join(TR.format_exception(value, chain=True)))
        ## ALT
        # err = "".join(TR.format_exception_only(_etype, value)).rstrip()
        # ## DISABLED: it seems they are already appended ?
        # # if value.__notes__:
        # #     err += "".join(NL + "  \\ " + note for note in value.__notes__)
        # log.error(err)
        # # OR: (fr.* for fr in __import__("traceback").extract_tb(tb))
        # due = value
        # while due:
        #     tb = TR.extract_tb(due.__traceback__)
        #     bt = "".join(TR.format_tb(tb)).rstrip().replace(NL, NL + "\\")
        #     log.info("Traceback (most recent call last):" + NL + "\\" + bt)
        #     due = due.__cause__

        ## ALSO:MAYBE:
        # _orig_excepthook(etype, value, tb)  # OR: sys.__excepthook__(...)
    finally:
        # log.config(write=_orig_write)
        log.error("</Exception>")  # TEMP


@contextmanager
def log_excepthook() -> (
    "Iterator[Callable[[Type[BaseException], BaseException, TracebackType], Any]]"
):
    _orig_excepthook = sys.excepthook
    try:
        sys.excepthook = exception_handler
        yield exception_handler
    finally:
        pass
        ## DISABLED: otherwise top-lvl exceptions won't be logged
        # sys.excepthook = _orig_excepthook
