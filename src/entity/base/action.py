from typing import TYPE_CHECKING, Callable, override

from .golden import Entities, Golden

if TYPE_CHECKING:
    from ...ui.view import EntityView


# ENH? inherit variety of `*Action and apply different color to each
#   * ActionItemMethod/Introspection
#   * ActionItemMROMethods/Inherited
#   * ActionItemGenerics/Aux/StandaloneAugmented/Registered/Annotated
#   * ActionVlst/ViewportListTransformations
#   * ActionKeys (or annotate above actions by binded keys)
class Action(Golden[str]):
    def __init__(
        self,
        name: str,
        pview: "EntityView",
        sfn: Callable[[], Entities],
    ) -> None:
        super().__init__(name, pview)
        self._sfn = sfn

    # TEMP:FIXED:ERR: Cannot instantiate abstract class "Action" with abstract attribute "loci"
    # @override
    # @property
    # def loci(self) -> str:
    #     return self._name

    @override
    def explore(self) -> Entities:
        return self._sfn()
