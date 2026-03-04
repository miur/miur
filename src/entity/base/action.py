from typing import Callable, override

from .golden import Entities, Entity, Golden


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
        parent: Entity,
        sfn: Callable[[], Entities],
    ) -> None:
        super().__init__(name, parent)
        self._sfn = sfn

    # TEMP:FIXED:ERR: Cannot instantiate abstract class "Action" with abstract attribute "loci"
    # @override
    # @property
    # def loci(self) -> str:
    #     return self._name

    @override
    def explore(self) -> Entities:
        return self._sfn()
