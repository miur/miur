import importlib

from . import base

__all__ = ['CommandMaker']


# NOTE: class instead of global list
# * supports multiple cmd sets
#   (E.G. for online hot-update by replacing whole class instead of locks
#       (in read-only haskell-like manner))
# + it's the must to move cmd/{base,factory} into .shared

# DEV:CHG: load from modules *.__all__ (if exist) or search all symbols for cmds
#   http://stackoverflow.com/questions/1057431/loading-all-modules-in-a-folder-in-python/8556471#8556471
#   https://docs.python.org/3.5/library/modules.html

class CommandMaker:
    def __init__(self, pkg):
        mod = importlib.import_module(pkg)
        self.cmds = {v.cmd: v for v in vars(mod).values()
                     if isinstance(v, type) and issubclass(v, base.BaseCommand)}

    def make(self, nm, ctx, *args):
        # THINK:WTF: if no such cmd ? Client will hang in infinite loop
        #   FIXME: BaseCommand => WrongCommand (generate exception and send back to client)
        ctor = self.cmds.get(nm, base.WrongCommand)
        return ctor(ctx, *args)
