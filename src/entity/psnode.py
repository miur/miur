from typing import override

from .base.golden import Entities, Entity, Golden
from .fsentry import FSAuto
from .text import TextEntry


class PSProto(Golden[str]):
    def __init__(self, text: str, parent: Entity) -> None:
        super().__init__(text, parent)

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
