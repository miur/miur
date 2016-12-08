from . import all as all_cmds
from . import base

__all__ = ['make_cmd']


# FIXME: don't use global var and save to class CommandDispatcher
#   => so I could support multiple cmd sets (E.G. for online hot-update by
#     replacing whole class instead of locks (in read-only haskell-like manner))
#   ++ It's the must to move command's general code into .shared
# Register all entries __class__.cmd in dict when loading
_cmdd = {v.cmd: v for v in vars(all_cmds).values()
         if isinstance(v, type) and issubclass(v, base.BaseCommand)}


def make_cmd(nm, *args):
    # THINK:WTF: if no such cmd ? Client will hang in infinite loop
    #   FIXME: BaseCommand => WrongCommand (generate exception and send back to client)
    CmdType = _cmdd.get(nm, base.WrongCommand)
    return CmdType(*args)
