import re
from typing import override

from ..integ.any_exe import run_bg_wait
from .base.golden import Entities, Entity, Golden
from .text import TextEntry


class HelpAutoExplorer(Golden[list[str]]):
    def __init__(self, cmd: list[str], help: str, parent: Entity) -> None:
        super().__init__(cmd, parent)
        self._help = help

    # IDEA: rename `Action to ".text()" in case of TextEntry, and .fsentry() if it's fs
    #   >> NICE: this way I can immediately glimpse "type" of default exploration based on obj `Type
    @override
    def explore(self) -> Entities:
        for s in run_bg_wait([*self._x, "--help"], split=True):
            if not s:
                # OR: allow <<H=0>> items as Atomic "placeholders/separators" w/o preview
                yield TextEntry(" ", self)
            elif m := re.match(r"^\s+(\w+)\s+(.*)", s):
                yield HelpAutoExplorer([*self._x, m[1]], m[2], self)
            else:
                yield TextEntry(s, self)

    # ALT:BET? inject Action(name=help) directly into the pool of `Actions to get immediate annotation
    #   ~~ though, current way makes it more "same" in approach
    def annot(self) -> str:
        return self._help

    @override
    @property
    def name(self) -> str:
        import shlex

        return shlex.join(self._x)


class ExperimentalHelpNode(Golden[str]):
    def __init__(self, parent: Entity) -> None:
        super().__init__("[--help]", parent)

    @override
    def explore(self) -> Entities:
        return [
            HelpAutoExplorer(["docker"], "Management of containers", self),
        ]
