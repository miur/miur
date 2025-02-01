from typing import TYPE_CHECKING, Callable, Iterable, Sequence, override

from .golden import Golden

if TYPE_CHECKING:
    from ...ui.view import EntityView


# RENAME? `Entity `Node
class Action(Golden):
    def __init__(
        self,
        name: str,
        pview: "EntityView",
        sfn: Callable[[], Sequence[Golden]],
    ) -> None:
        self._name = name
        self._sfn = sfn
        super().__init__(pview)

    @override
    @property
    def name(self) -> str:
        return self._name

    # TEMP:FIXED:ERR: Cannot instantiate abstract class "Action" with abstract attribute "loci"
    @override
    @property
    def loci(self) -> str:
        return self._name

    @override
    def explore(self) -> Iterable[Golden]:
        return self._sfn()
