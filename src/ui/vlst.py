from .vlst_base import SatelliteViewportBase
from .vlst_redraw import SatelliteViewport_RedrawMixin
from .vlst_stepby import SatelliteViewport_StepbyMixin


# REF: https://stackoverflow.com/questions/10018757/how-does-the-order-of-mixins-affect-the-derived-class
#   (order): same as "class Viewport(Stepby(Redraw(Base))"
class SatelliteViewport(
    SatelliteViewport_StepbyMixin,
    SatelliteViewport_RedrawMixin,
    SatelliteViewportBase,
):
    pass
