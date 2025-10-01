from typing import override

from ..integ.any_exe import run_bg_wait
from .base.golden import Entities, Entity, Golden
from .text import TextEntry


class DockerCmdEntry(Golden[str]):
    def __init__(self, text: str, parent: Entity) -> None:
        super().__init__(text, parent)

    def _run(self, cmdline: str) -> Entities:
        cmdv = [*cmdline.split(), self.name]
        nl = run_bg_wait(cmdv, split=True)
        assert nl
        return (TextEntry(l, self) for l in nl if l)

    @override
    def explore(self) -> Entities:
        return self._run("pacman -Qi")

    def pacl(self) -> Entities:
        return self._run("pacman -Ql")


class DockerClientProto(Golden[str]):
    def __init__(self, text: str, parent: Entity) -> None:
        super().__init__(text, parent)

    @override
    def explore(self) -> Entities:
        from .objaction import ObjAction

        return [
            ObjAction(
                name="images",
                parent=self,
                fn=lambda: run_bg_wait(["docker", "images"]),
            ),
        ]


# MAYBE?(don't remember): put this into rootnode.py to be able to live-code above classes in sep module ?
class DockerNode(Golden[str]):
    def __init__(self, parent: Entity) -> None:
        super().__init__("[docker]", parent)

    @override
    def explore(self) -> Entities:
        return [
            DockerClientProto("client", self),
        ]
