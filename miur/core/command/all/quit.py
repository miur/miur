from ..base import BaseCommand

__all__ = ['QuitCmd']


class QuitCmd(BaseCommand):
    cmd = 'quit-all'

    def __init__(self, *args):
        pass

    # TEMP:HACK: reflect 'quit' back to rotate cycle once more until false condition
    def execute(self):
        pass
