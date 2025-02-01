from typing import TYPE_CHECKING, override

from .autodiscover import AutoRegistered
from .traits import Standart

if TYPE_CHECKING:
    from ...ui.view import EntityView


# RENAME? `Entity `Node
# TEMP: to focus_on(match-case Golden())
class Golden(Standart, AutoRegistered):
    def __init__(self, pview: "EntityView") -> None:
        self._pv = pview

    @override
    @property
    def pv(self) -> "EntityView":
        return self._pv

    @override
    def __repr__(self) -> str:
        loci = self.loci
        if not loci.endswith(nm := self.name):
            loci = nm + loci
        if loci.startswith("/"):
            return f"`{loci}"
        return f"{type(self).__name__}({loci})"
