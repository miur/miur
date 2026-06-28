import sys
from collections.abc import Callable, Generator
from contextlib import contextmanager
from types import TracebackType
from typing import Any


def exception_handler(
    _etype: type[BaseException],
    value: BaseException,
    _tb: TracebackType | None,
) -> None:
    import traceback

    te = traceback.TracebackException.from_exception(value, compact=True)
    text = "".join(te.format())
    try:
        from .. import log

        log.failure(text)
    except Exception:
        print(text, file=sys.stderr)


@contextmanager
def set_excepthook() -> Generator[
    Callable[[type[BaseException], BaseException, TracebackType], Any]
]:
    _orig_excepthook = sys.excepthook
    try:
        sys.excepthook = exception_handler
        yield exception_handler
    finally:
        ## DISABLED: otherwise top-lvl exceptions won't be logged
        # sys.excepthook = _orig_excepthook
        pass
