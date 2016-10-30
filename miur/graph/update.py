import re
import threading
from miur.graph.dispatch import Dispatcher

dsp = Dispatcher()


def dispatch(self, cmd, *args):
    # TEMP: normalize command name
    cmd = re.sub(r'\W', '_', cmd)
    # ALT: bound = self.dispatch_map[cmd].__get__(self, type(self))
    # SEE: http://stackoverflow.com/questions/19075843/dispatch-a-class-method
    f = getattr(self, cmd, '_err_wrong_cmd')
    return f(*args)


def update(cmd):
    if cmd == 'quit':
        return True
    # BAD: Lock on whole dom NEED: individual on each operation
    #   THINK lock read operation also? Until value was changed.
    #   ALSO: wtf dir list changed (synced) directly after op ?
    with threading.Lock():
        dispatch(dsp, cmd)
