import os
import sys
from contextlib import contextmanager
from typing import Iterator

from .. import _pkg
from .logger import log


def pidfile_path() -> str:
    # OR:(/tmp)=f"/run/user/{os.getlogin()}" os.environ.get('', "/tmp")
    return os.environ.get("XDG_RUNTIME_DIR", "/tmp") + "/" + _pkg.__appname__ + ".pid"


@contextmanager
def temp_pidfile(pidfile: str) -> Iterator[None]:
    with open(pidfile, "w", encoding="utf-8") as f:
        f.write(str(os.getpid()))
    try:
        yield
    finally:
        os.remove(pidfile)


def send_pidfile_signal(pidfile: str, sig: int) -> None | ProcessLookupError:
    # MAYBE: check by short LOCK_EX if any LOCK_SH present (i.e. main miur running)
    try:
        # SEIZE: trbs/pid: Pidfile featuring stale detection and file-locking ⌇⡦⠿⣢⢔
        #   https://github.com/trbs/pid
        with open(pidfile, "r", encoding="utf-8") as f:
            # [_] TODO: test /proc/pid/exe is the same as /proc/self/exe (OR: sys.argv[0])
            pid = int(f.read())
    except FileNotFoundError as exc:
        log.error(f"fail {pidfile=} | {exc}")
        sys.exit(1)

    log.warning(f"sending signal={sig} to {pid=}")
    try:
        os.kill(pid, sig)
        return None
    except ProcessLookupError as exc:
        log.error(f"fail {pid=} | {exc}")
        return exc
