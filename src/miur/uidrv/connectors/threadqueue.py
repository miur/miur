import sys
from collections.abc import Callable, Generator
from contextlib import contextmanager
from queue import Empty, Queue

from ... import log


@contextmanager
def new_guithread() -> Generator[
    tuple[Callable[[object], None], Queue[dict[str, object]]]
]:
    """
    Context manager that spins up a PySide6 GUI thread and yields a thread-safe
    send function and a receive queue.
    """
    from threading import Event, Thread

    # OLD:RENAME? (to_qt,from_qt) | (sendqt,recvqt)
    send_bridge: list[Callable[[object], None]] = []
    recv_q: Queue[dict[str, object]] = Queue()

    # Synchronization event to prevent yielding before Qt is ready
    ready_event = Event()

    def _qt_tgt() -> None:
        # RQ: all Qt stuff MUST be imported in GUI thread first
        from ..qt6wdg_drv import main

        main(send_bridge, recv_q, ready_event)

    qthr = Thread(target=_qt_tgt, daemon=True)  # , args=(to_qt, from_qt)
    qthr.start()

    # Corner Case: Block main thread until the GUI thread completes QApplication setup
    ready_event.wait(timeout=3)
    if not send_bridge:
        # TEMP:HACK: crash on access to empty list if Qt had failed
        raise RuntimeError("Qt seems to crash?")

    def send_fn(item: object) -> None:
        # Corner Case: Prevent calls if the emitter has been cleared during shutdown
        if send_bridge:
            send_bridge[0](item)

    try:
        # Yield the bound signal.emit directly. Main thread sees Callable[[Any], None].
        yield (send_fn, recv_q)
    finally:
        exit_cookie = "SHUTDOWN"
        send_fn(exit_cookie)
        try:
            while recv_q.get(timeout=3)["ev"] != exit_cookie:
                pass
            log.info(exit_cookie)
        except Empty:
            log.warning("WARN: can't safely close Qt")

        ## WARN: don't use Threads [which living less than MainThread] for UI
        ##   as Qt GUI-thread should be never destroyed! (even after app.quit())
        # WARN:ARCH: actually, this thread should NEVER end after first ever Qt import
        #   >> if you wish to recreate Qt window AGAIN -- you need *same* thread
        #   IDEA: only join on !miur exit, and all other time -- keep thread running in a loop,
        #     and waiting for new msgs from send_fn() to re-spawn Qt app again
        qthr.join(timeout=3)
        log.info("join")
        if qthr.is_alive():
            print("WARN: qt thread is running aft timeout", file=sys.stderr)
