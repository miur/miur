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

        ## HACK: initiate forceful refresh() at startup as if we got SIGWINCH
        os.write(_wsigfd, int.to_bytes(signal.SIGWINCH, 1))

        ## WARN: required, otherwise .set_wakeup_fd() won't be triggered due its SIG_DFL=SIG_IGN
        ##   BAD: still KEY_RESIZE is not delivered until you press any key due to Epoll
        signal.signal(signal.SIGWINCH, lambda si,fr: None)
        # signal.signal(signal.SIGWINCH, lambda si, fr: log.warning(
        #         f"{signal.Signals(si).name}={si}: {signal.strsignal(si)} during <{fr.f_code.co_filename}:{fr.f_lineno}>"
        # ))

        yield _rsigfd
    finally:
        os.close(_rsigfd)
        os.close(_wsigfd)
