from typing import TYPE_CHECKING, override

from .base.golden import Entities, Entity
from .fsentry import FSDir

if TYPE_CHECKING:
    from ...ui.view import EntityView


# RENAME?~ {Root,Central,Menu}Entry
#   BET? rename all top-lvl providers to `*Node ex~: 'FSEntry("/")' -> "FSNode"
class RootNode(Entity):
    def __init__(self, pview: "EntityView") -> None:
        # NICE: makes distinct prefix before adding "/" of "file:"
        # BAD: takes premium space ALT:USE: 'name=""'
        # IDEA: show this name-prefix only when navigating to `RootNode,
        #   orse hide it and start from chosen protocol onwards
        # BET:CHG:(pview)=self=_pool[RootNode] -> EntityView
        super().__init__("miur://", pview)

    @override
    @property
    def loci(self) -> str:
        return ""  # NOTE:(""): it's sole non-ambiguous loci for central menu

    @override
    def explore(self) -> Entities:
        # OR: do we really need "file://" ?
        #   isn't it prolifiration from "http://" ? -- which is wrong and should had been "http:"
        return [FSDir("/", pview=self._pv)]  # , nm="file:"
