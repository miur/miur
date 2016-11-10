import re
import logging
import threading

from miur.cursor.dispatch import Dispatcher
from miur.ui import client
from miur.cursor import state, command

dsp = Dispatcher()
_log = logging.getLogger(__name__)


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
        execute(command.ListNode(state.path))
        return
    if cmd == 'quit':
        return True
    # BAD: Lock on whole dom NEED: individual on each operation
    #   THINK lock read operation also? Until value was changed.
    #   ALSO: wtf dir list changed (synced) directly after op ?
    with threading.Lock():
        dispatch(dsp, cmd)


def execute(cmd_obj):
    client.put_cmd_threadsafe(cmd_obj)
