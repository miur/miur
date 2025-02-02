import os

from ..util.logger import log
from .any_exe import run_tty_async, to_cmdv


# [_] #visual FIXME: don't dump logs into shell on altscreen, when jumping into fullscreen app
#   &why it creates significant delay and glimpses text behind the screen unnecessarily
#   ALT: force somehow TERM to not refresh screen contents when doing altscreen with app
#   ALSO: same when you quit ranger -- it should exit "less cleanly" to not repaint the TERM
def run_ranger(path: str, *args: str) -> None:
    if int(v := os.getenv("RANGER_LEVEL", "0")) > 0:
        log.warning("skipping cmd due to RANGER_LEVEL=" + v)
        return  # NOTE: exit ranger-nested shell

    import os.path as fs

    # MAYBE: strip loci to be an actual path (even for text file lines)
    if not fs.exists(path):
        raise ValueError(path)

    # TRY: replace XDG file by some e.g. fd=3 redir
    tmp = os.getenv("XDG_RUNTIME_DIR", "/tmp") + "/ranger/cwd"

    def _cb() -> None:
        # TRY:BET? inof per-app filecache do 'readlink /proc/<child>/cwd' on child process
        #   XLR: can we handle SIGCHLD by background asyncio and read cwd from dying child ?
        with open(tmp, encoding="utf-8") as f:
            cwd = f.read(1024)

        # pylint:disable=protected-access
        if cwd != fs.dirname(path):  # OR: g.root_wdg._navi._view._ent.loci
            from ..app import g_app
            from ..curses_ext import resize
            from ..entity.fsentry import FSAuto

            # ALT: if os.exists(cwd) and cwd != os.getcwd(): os.chdir(cwd)
            g_app.root_wdg._navi.view_jump_to(FSAuto(cwd, None))
            resize()  # <COS: term could have been resized when using nested app

    # INFO:(--selectfile): it's obsoleted by pos-args, according to "man ranger"
    #   BUT! if you pass a dir in pos-args -- it will be *opened*, inof *selected*
    # [_] TRY:ENH: inof "choosedir" make ranger write last file under cursor
    #   => so I could jump !miur to same file, i.e. sync position of cursor itself
    cmdv = to_cmdv("ranger", {"choosedir": tmp, "selectfile": path}, args)
    run_tty_async(cmdv, cb=_cb)
