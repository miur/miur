import logging

from . import bus
from . import command
from .server import ClientProtocol

_log = logging.getLogger(__name__)


_cmdd = {v.cmd: v for v in vars(command).values()
         if isinstance(v, type) and issubclass(v, command.BaseCommand)}


async def executor():
    while True:
        cid, (ifmt, obj) = await bus.qin.get()
        _log.debug('Command: {!r}'.format(obj))

        if obj['cmd'] == 'quit-all':
            # TEMP:HACK: reflect 'quit' back to rotate cycle once more until false condition
            r = None
        else:
            # ALT: Register all entries __class__.cmd in dict when loading
            C = _cmdd.get(obj['cmd'])
            c = C(*obj['args'])
            r = c.execute()

        # THINK:WTF: if no such cmd ? Client will hang in infinite loop
        _log.debug('Results: {!r}'.format(r))

        # THINK: instead of directly send msg you can save it to self._msg and
        #   later process this object in send queue
        rsp = {'id': obj['id'], 'rsp': r}
        await ClientProtocol.send(cid, rsp, ifmt)

        if obj['cmd'] == 'quit-all':
            break
