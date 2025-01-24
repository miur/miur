from ..util.logger import log
from .any_exe import run_bg_wait

g_cmdv: tuple[str, ...]


def to_system_clipboard(text: str, /) -> None:
    if "g_cmdv" not in globals():
        from shutil import which

        global g_cmdv

        if which("xci"):
            g_cmdv = ("xci",)
        elif which("xclip"):
            g_cmdv = ("xclip", "-selection", "clipboard")
    log.info((g_cmdv, text))
    run_bg_wait(g_cmdv, input=text)
