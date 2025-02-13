from typing import override

from .base.golden import Entities, Entity, Golden


# ADD: host/external (e.g. SSH)
class LocalHostNode(Golden[str]):
    def __init__(self, parent: Entity) -> None:
        super().__init__("[localhost]", parent)

    @override
    def explore(self) -> Entities:
        from .fsentry import FSDir
        from .pacmannode import PacmanProto
        from .psnode import PSProto

        return [
            FSDir("/", self),
            PSProto("&ps", self),
            PacmanProto("&pacman", self),
            # ADD: LsPci/LsUsb/journalctl/pacman
        ]


# RENAME? miur/internal
class MiurAppNode(Golden[str]):
    def __init__(self, parent: Entity) -> None:
        super().__init__("[!miur]", parent)

    @override
    def explore(self) -> Entities:
        import os

        from ..app import g_app
        from ..keymap import spawn_render
        from .objaction import ObjAction

        return [
            # NICE:IDEA: inof exposing each subsystem of !miur one-by-one,
            #   BET: directly expose whole "g_app" and *make its structure easy to access/navigate*
            #   i.e. it should look the same during programming and during navi()
            ObjAction(name="@env", parent=self, fn=lambda: os.environ),
            ObjAction(name="@g_app", parent=self, fn=lambda: vars(g_app)),
            ObjAction(name="@keybindings", parent=self, fn=lambda: g_app.keytableroot),
            ObjAction(
                name="@demo/render",
                parent=self,
                fn=lambda: [
                    ObjAction(name=nm, parent=self, fn=lambda nm=nm: spawn_render(nm))
                    for nm in [
                        "glfw_imgui",
                        "qt6gl",
                        "qt6qml",
                        "pyqtgr_numpy",
                        "sdl3gl_imgui",
                        "qt6wg",
                    ]
                ],
            ),
        ]


class ProtocolNode(Golden[str]):
    def __init__(self, parent: Entity) -> None:
        super().__init__("[protocols]", parent)

    @override
    def explore(self) -> Entities:
        from .mpdnode import MPDProto

        return [MPDProto("&mpd", self)]


class WebNode(Golden[str]):
    def __init__(self, parent: Entity) -> None:
        super().__init__("[www]", parent)

    @override
    def explore(self) -> Entities:
        from .webnode import WebPageEntity

        return [
            WebPageEntity("http://example.com/", self),
            WebPageEntity("https://docs.python.org/3/library/os.path.html", self),
        ]


class DatasetNode(Golden[str]):
    def __init__(self, parent: Entity) -> None:
        super().__init__("[datasets]", parent)

    @override
    def explore(self) -> Entities:
        from .unicodenode import UnicodeNode

        return [
            UnicodeNode("*unicode", self),
            # ADD: ANSITermColorsNode("*termansi", self),
        ]


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

        # OR: do we really need "file://" ?
        #   isn't it prolifiration from "http://" ? -- which is wrong and should had been "http:"
        # ALT: nm="file:"
        return [
            LocalHostNode(self),
            MiurAppNode(self),
            ProtocolNode(self),
            WebNode(self),
            DatasetNode(self),
        ]
