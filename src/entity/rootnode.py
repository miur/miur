from typing import Iterable, override

from .base import Golden
from .fsentry import FSEntry


# RENAME?~ {Root,Central,Menu}Entry
#   BET? rename all top-lvl providers to `*Node ex~: 'FSEntry("/")' -> "FSNode"
class RootNode(Golden):
    def __init__(self) -> None:
        pass

    @override
    @property
    def name(self) -> str:
        return ""

    @override
    @property
    def loci(self) -> str:
        return ""  # NOTE:(""): it's sole non-ambiguous loci for central menu

    @override
    def explore(self) -> Iterable[Golden]:
        # OR: do we really need "file://" ?
        #   isn't it prolifiration from "http://" ? -- which is wrong and should had been "http:"
        return [FSEntry("/", nm="file:")]
