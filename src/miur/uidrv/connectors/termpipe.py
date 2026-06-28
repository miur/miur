import os
import sys
import tempfile
import time
from collections.abc import Generator
from contextlib import ExitStack, contextmanager
from subprocess import Popen, TimeoutExpired
from typing import TextIO

from ... import log


# REF:PRIA: /d/miur/legacy/bc5_miur_asyncio/devhelp/newterm.py
#   BET? /d/miur/&g/newterm.nou
#     SEE: dedicated .py script to send IO/events back'n'forth
@contextmanager
def new_termwindow() -> Generator[tuple[TextIO, TextIO]]:
    with ExitStack() as stack:
        tmpf = stack.enter_context(tempfile.NamedTemporaryFile(mode="r"))
        cmd = 'tty > "$0"; trap "kill -WINCH $1" WINCH; inotifywait -qq -e delete_self "$0"'
        bgtty = stack.enter_context(
            # DISABLED:("-M"): I need to scroll history up
            #   BAD:(need -M): orse tmux from newterm issues esccode into mainterm tmux,
            #     which prints ERR: erresc: unknown private set/reset mode 2031
            Popen(
                "st -n miur -t miur_textstream -e sh -c".split()
                + [cmd, tmpf.name, str(os.getpid())]
            )  # nosec: B603
        )

        def _cleanup_terminal() -> None:
            log.info("un-term")
            try:
                bgtty.wait(timeout=0.2)
            except TimeoutExpired:
                ## WARN: force terminal death before ExitStack tries to wait on it!
                bgtty.terminate()

        stack.callback(_cleanup_terminal)
        ## HACK: close file to exit remote terminal *before* terminating
        stack.callback(tmpf.close)

        time.sleep(0.3)
        tmpf.seek(0)
        ttynm = tmpf.read().strip()

        # pylint:disable=consider-using-with
        rtty = open(ttynm, encoding=sys.stdin.encoding)
        stack.callback(rtty.close)
        wtty = open(ttynm, "w", encoding=sys.stdout.encoding)
        stack.callback(wtty.close)
        yield (rtty, wtty)
