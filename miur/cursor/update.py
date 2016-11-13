import re
import logging
import threading

from miur.cursor.dispatch import Dispatcher
from miur.ui import client
from miur.cursor import message

dsp = Dispatcher()
_log = logging.getLogger(__name__)

# SEE /ranger/api/commands.py:38
_msgd = {v.cmd: v for v in vars(message).values()
         if isinstance(v, type) and issubclass(v, message.BaseMessage)}


# TODO: replace by constructing 'msg' from _msgd{} classes
def dispatch(self, cmd, *args):
    _log.info("Cmd: {}".format(cmd))
    # TEMP: normalize command name
    cmd = re.sub(r'\W', '_', cmd)
    # ALT: bound = self.dispatch_map[cmd].__get__(self, type(self))
    # SEE: http://stackoverflow.com/questions/19075843/dispatch-a-class-method
    f = getattr(self, cmd, '_err_wrong_cmd')
    return f(*args)


def update(cmd):
    if cmd == '_init':
        handle(message.ListNodeMsg())
        return
    if cmd == 'quit':
        return True
    # BAD: Lock on whole dom NEED: individual on each operation
    #   THINK lock read operation also? Until value was changed.
    #   ALSO: wtf dir list changed (synced) directly after op ?
    with threading.Lock():
        dispatch(dsp, cmd)


# NOTE: wrapper to async 'schedule' and sync 'process' functions
def handle(msg):
    client.put_cmd_threadsafe(msg)
