import logging

from . import bus
from . import command

_log = logging.getLogger(__name__)


_cmdd = {v.cmd: v for v in vars(command).values()
         if isinstance(v, type) and issubclass(v, command.BaseCommand)}


# DEV:MAYBE:(split!) there is sense in constructing cmds directly on receiving
#   => so I can immediately construct QuitMsg and then decide when to execute it
#       (immediately or after rest of queue -- to support quit_now and graceful_exit)
#   =>> then executor() will be only calling '.execute()' method and qout.put()

async def executor():
    while True:
        cid, (ifmt, obj) = await bus.qin.get()
        _log.debug('Command: {!r}'.format(obj))

        # ALT: Register all entries __class__.cmd in dict when loading
        # ? THINK:WTF: if no such cmd ? Client will hang in infinite loop
        C = _cmdd.get(obj['cmd'])
        c = C(*obj['args'])
        r = c.execute()

        rsp = {'id': obj['id'], 'rsp': r}
        bus.qout.put_nowait((cid, (ifmt, rsp)))
        bus.qin.task_done()
