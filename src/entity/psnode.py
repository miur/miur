from typing import override

from .base.golden import Entities, Entity, Golden
from .fsentry import FSAuto
from .text import TextEntry


class PSProto(Golden[str]):
    def __init__(self, text: str, parent: Entity) -> None:
        super().__init__(text, parent)

    def unknown(self) -> Entities:
        # USAGE: $ ln -svt /d/miur/.venv/lib/python3.12/site-packages -- /d/just
        from just.tools.proc.ps import only_unknown

        for ps in only_unknown():
            yield TextEntry(f"{ps.pid}: {ps.name()} {ps.cmdline()}", self)

    @override
    def explore(self) -> Entities:
        import psutil

        # psutil.pids()
        # psutil.pid_exists(32498)
        # ps = psutil.Process(32498)
        # p.name()
        # p.cmdline()
        # p.terminate()
        # p.wait()
        for ps in psutil.process_iter():
            # yield FSAuto(str(ps), self)
            yield TextEntry(f"{ps.pid}: {ps.name()} {ps.cmdline()}", self)
