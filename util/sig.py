import fcntl
import os
import signal
from contextlib import contextmanager
from typing import Iterator


def fd_flags(*fds: int, add: int = 0, rem: int = 0) -> tuple[int, ...]:
    if add or rem:
        for fd in fds:
            flags = fcntl.fcntl(fd, fcntl.F_GETFL)
            if add:
                flags |= add
            if rem:
                flags &= ~rem
            fcntl.fcntl(fd, fcntl.F_SETFL, flags)
    return fds


@contextmanager
def route_signals_to_fd() -> Iterator[int]:
    try:
        _rsigfd, _wsigfd = fd_flags(*os.pipe(), add=os.O_NONBLOCK)
        _orig_fd = signal.set_wakeup_fd(_wsigfd, warn_on_full_buffer=True)
        assert _orig_fd == -1
        _orig_hdl = signal.signal(signal.SIGWINCH, signal.SIG_DFL)
        yield _rsigfd
    finally:
        os.close(_rsigfd)
        os.close(_wsigfd)
