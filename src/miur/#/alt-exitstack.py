import curses as C
from collections.abc import Callable
from types import TracebackType
from typing import Self


class CursesUIDriver:
    stdscr: C.window

    def __init__(self) -> None:
        self._cleanup_callbacks: list[Callable[[], object]] = []

    def _configure(self) -> None:
        __ = self._cleanup_callbacks.append  # RENAME? _defer()
        C.setupterm()
        self.stdscr = C.initscr()
        __(C.endwin)
        C.raw()
        __(C.noraw)

    # NOTE~ `ExitStack does the same better, but returnes chained exc.__context__
    #   ~~ RND: ExceptionGroup is more reasonable, considering cleanup exc are mostly independent
    #     BUT: I think current support for chained ctx is more mature, so let's use ExitStack anyway
    def _restore(self) -> ExceptionGroup | None:
        errors: list[Exception] = []
        interrupts: list[BaseException] = []
        # ALT: while self._cleanup_callbacks: cleanup_fn = self._cleanup_callbacks.pop()
        for fn in reversed(self._cleanup_callbacks):
            try:
                fn()
            except Exception as exc:
                # CASE: run all cleanups, even if some of them are failing
                #   >> we should try to restore terminal as much as we can
                errors.append(exc)
            except BaseException as cfl:
                # WARN:(BaseException): teardown even on Ctrl+C (KeyboardInterrupt) or forced exit (SystemExit)
                interrupts.append(cfl)
        if interrupts:
            # FIXME? chain them to each other (e.g. Ctrl+C during SystemExit)
            raise interrupts[0] from None
        self._cleanup_callbacks.clear()
        return ExceptionGroup("cleanup failures", errors) if errors else None

    def __enter__(self) -> Self:
        try:
            self._configure()
        except Exception as setup_exc:
            # FIXED: crash in __enter__ doesn't call __exit__
            if restore_grp := self._restore():
                # BAD: should be py:$ raise restore_grp from setup_exc
                #   BUT! we want to keep "setup_exc" as "primary error"
                raise setup_exc from restore_grp
            raise
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> None:
        restore_exc = self._restore()
        if restore_exc:
            if exc:
                ## DISABLED: we want to keep "exc" as "primary error"
                # raise restore_exc from exc
                exc.__context__ = restore_exc
            else:
                raise restore_exc
