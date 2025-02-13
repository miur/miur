from typing import override

from ..integ.any_exe import run_bg_wait
from .base.golden import Entities, Entity, Golden
from .fsentry import FSAuto
from .text import TextEntry


class PackageEntry(Golden[str]):
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

    def pacR1o(self) -> Entities:
        # FIXME: "--color" doesn't work in my curses-translator
        return self._run("pactree --depth 1 --optional --reverse")

    def pacr1o(self) -> Entities:
        return self._run("pactree --depth 1 --optional")

    def pacr1(self) -> Entities:
        return self._run("pactree --depth 1")

    def pacr2(self) -> Entities:
        return self._run("pactree --depth 2")

    def pacr(self) -> Entities:
        return self._run("pactree")

    def pacRo(self) -> Entities:
        return self._run("pactree --optional --reverse")


class PacmanProto(Golden[str]):
    def __init__(self, text: str, parent: Entity) -> None:
        super().__init__(text, parent)

    @override
    def explore(self) -> Entities:
        # from .objaction import ObjAction
        # return [
        #     ObjAction(
        #         name="explicit",
        #         parent=self,
        #         # TODO:SEIZE: /d/miur/_try/bc5_miur_asyncio/dom/provider.py
        #         fn=lambda: run_bg_wait(["pacman", "-Qqe"]),
        #     )
        # ]
        #
        for nm in run_bg_wait(["pacman", "-Qqe"], split=True):
            yield PackageEntry(nm, self)
