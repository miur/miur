from typing import Callable, override

from .golden import Entities, Entity, Golden


# RENAME? `Closure(obj, "method", xfmr:code)
#   * EntityAction
#   * DerivedEntityAction
#   * LambdaAction
#   * ObjAction (basically, can wrap all others)
# ENH? inherit variety of `*Action and apply different color to each
#   * ActionItemMethod/Introspection
#   * ActionItemMROMethods/Inherited
#   * ActionItemGenerics/Aux/StandaloneAugmented/Registered/Annotated
#   * ActionVlst/ViewportListTransformations
#   * ActionKeys (or annotate above actions by binded keys)
class Action(Golden[str]):
    __slots__: tuple[str, ...] = ("_sfn",)
    _sfn: Callable[[], Entities]

    def __init__(
        self,
        name: str,
        parent: Entity,
        sfn: Callable[[], Entities],
    ) -> None:
        super().__init__(name, parent)
        # BET?(less indirection == directly assign): self.explore = sfn
        self._sfn = sfn

    # TEMP:FIXED:ERR: Cannot instantiate abstract class "Action" with abstract attribute "loci"
    # @override
    # @property
    # def loci(self) -> str:
    #     return self._name

    # FIXME: return internal structure by .explore() the closure
    #   ADD: use ".execute() -> object" to actually execute it
    #   MAYBE: backport .execute() into `Entity and eliminate `Action itself
    #     i.e. `Closure will become `Accessor with "sfn" being an unique memref into AppPythonSystem
    #       with assoc .name passed during Accessor creation
    @override
    def explore(self) -> Entities:
        return self._sfn()
