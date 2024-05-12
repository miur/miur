import os
import sys
from contextlib import contextmanager
from types import TracebackType
from typing import Any, Callable, Iterator, Type

from .log import log


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
        log.error("".join(TR.format_exception_only(etype, value)).rstrip())
        # _orig_excepthook(etype, value, tb)  # OR: sys.__excepthook__(...)
    finally:
        log.config(write=_orig_write)


@contextmanager
def custom_excepthook_log() -> (
    Iterator[Callable[[Type[BaseException], BaseException, TracebackType], Any]]
):
    _orig_excepthook = sys.excepthook
    try:
        sys.excepthook = exception_handler
        yield exception_handler
    finally:
        sys.excepthook = _orig_excepthook
