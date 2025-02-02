from typing import Iterable, override

from .base import Golden
from .fsentry import FSEntry


# RENAME?~ {Root,Central,Menu}Entry
#   BET? rename all top-lvl providers to `*Node ex~: 'FSEntry("/")' -> "FSNode"
class RootNode(Golden):
    def __init__(self) -> None:
        # NICE: makes distinct prefix before adding "/" of "file:"
        # BAD: takes premium space ALT:USE: 'name=""'
        # IDEA: show this name-prefix only when navigating to `RootNode,
        #   orse hide it and start from chosen protocol onwards
        # BET:CHG:(pview)=self=_pool[RootNode] -> EntityView
        super().__init__("miur://", pview=None)

    @override
    @property
    def loci(self) -> str:
        return ""  # NOTE:(""): it's sole non-ambiguous loci for central menu

    @override
    def explore(self) -> Iterable[Golden]:
        # OR: do we really need "file://" ?
        #   isn't it prolifiration from "http://" ? -- which is wrong and should had been "http:"
        return [FSEntry("/", nm="file:")]
