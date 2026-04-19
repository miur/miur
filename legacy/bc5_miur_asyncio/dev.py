# MAYBE: define common interface

from abc import ABC, abstractmethod


class InputIfc(ABC):
    # name: str
    @property
    @abstractmethod
    def attach(self) -> None:
        pass

    # @final
    # def __int__(self) -> int:
    #     return int(self.sum.total_seconds())


class OutputIfc(ABC):
    # name: str
    @property
    @abstractmethod
    def attach(self) -> None:
        pass

    # @final
    # def __int__(self) -> int:
    #     return int(self.sum.total_seconds())


# NOTE: single Device is physically still two separate ones
#   e.g. monitor+keyboard, or display+sensor
#     == they are combined only because usually then are both destroyed at the same time
class Device(OutputIfc, InputIfc):
    pass


class InputDevice(InputIfc):
    def __init__(self, dev: Device):
        self.dev = dev


class OutputDevice(OutputIfc):
    def __init__(self, dev: Device):
        self.dev = dev


class CursesDevice(Device):
    pass


# IDEA: all related keypress-sources should refer single "keyboard+layout" device backend to unify processing
#   BET? instead of backend create generic "Keybindings" frontend,
#     COS each Input device will need translate minor differences into same keybinds
class CursesInput(InputDevice):
    def __init__(self, dev: Device):
        super().__init__(dev)


class CursesOutput(OutputDevice):
    def __init__(self, dev: Device):
        super().__init__(dev)
