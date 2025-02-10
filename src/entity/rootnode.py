import os
from typing import override

from .base.golden import Entities, Golden


# RENAME?~ {Root,Central,Menu}Entry
#   BET? rename all top-lvl providers to `*Node ex~: 'FSEntry("/")' -> "FSNode"
class RootNode(Golden[str]):
    def __init__(self) -> None:
        # NICE: makes distinct prefix before adding "/" of "file:"
        # BAD: takes premium space ALT:USE: 'name=""'
        # IDEA: show this name-prefix only when navigating to `RootNode,
        #   orse hide it and start from chosen protocol onwards
        # BET:CHG:(pview)=self=_pool[RootNode] -> EntityView
        super().__init__("miur://", self)

    @override
    @property
    def loci(self) -> str:
        return ""  # NOTE:(""): it's sole non-ambiguous loci for central menu

    @override
    def explore(self) -> Entities:
        from ..app import g_app
        from .fsentry import FSDir
        from .mpdnode import MPDProto
        from .objaction import ObjAction
        from .psnode import PSProto

        # OR: do we really need "file://" ?
        #   isn't it prolifiration from "http://" ? -- which is wrong and should had been "http:"
        # ALT: nm="file:"
        return [
            FSDir("/", self),
            ObjAction(name="@env", parent=self, fn=lambda: os.environ),
            ObjAction(name="@g_app", parent=self, fn=lambda: vars(g_app)),
            ObjAction(name="@keybindings", parent=self, fn=lambda: g_app.keytableroot),
            MPDProto("&mpd", self),
            PSProto("&ps", self),
        ]
