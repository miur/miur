from typing import override

from .base.golden import Entities, Entity, Golden, StopExploration


class TextEntry(Golden[str]):
    __slots__ = ()  # "name", "loci", "explore")

    # def __init__(self, text: str, loci: tuple[str, ...] | None = None) -> None:
    def __init__(self, text: str, parent: Entity) -> None:
        super().__init__(text, parent)
        # self._at = loci  # = (path,lnum,[col,[boff,...]])

    # @override
    # @property
    # def loci(self) -> str:
    #     return "".join(self._at) if self._at else "∅ " + repr(self)

    @override
    def explore(self) -> Entities:
        from re import finditer

        cls = type(self)
        words = [
            cls(
                m.group(0),
                # loci=(
                #     (
                #         ## DISABLED: interferes with !nvim jumping to line under cursor
                #         # self._at[0],
                #         # f":{m.start()}+{m.end() - m.start()}",
                #         # self._at[-1],
                #         *self._at,
                #         f":{m.start()+1}",
                #     )
                #     if self._at
                #     else ("∅", f":{m.start()}+{m.end() - m.start()}")
                # ),
            )
            for m in finditer(r"\S+", self._x)
        ]
        if len(words) == 1:
            raise StopExploration()
            # return [ErrorEntry("INDIVISIBLE WORD", loci=self._at)]
        return words
