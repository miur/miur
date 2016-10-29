import threading

# from miur.relay import client
import miur.access.fs as graph

# tcp, unix, mem_share, etc
coupling = None

# Server addr
saddr = None

cursor = 0
path = '/'
entries = graph.list_nodes(path)


def shift(ndir):
    global entries, cursor, path

    if ndir == 'right' and cursor is None:
        return

    if ndir == 'left':
        path = graph.parent_node(path)
    elif ndir == 'right':
        path = graph.child_node(path, entries[cursor])

    entries = graph.list_nodes(path)
    cursor = 0 if entries else None


# TODO: class with methods and name conversion table
#   http://stackoverflow.com/questions/19075843/dispatch-a-class-method
def dispatch(self, arg):
    bound = self.dispatch_map[arg].__get__(self, type(self))
    bound()


def docmd(cmd):
    global entries, cursor, path

    # if coupling == 'tcp':
    # ALT: entries = client.send(saddr, path)

    if cursor is not None and entries is not None:
        if cmd == 'focus-node-next':
            cursor = min(cursor + 1, len(entries) - 1)
        elif cmd == 'focus-node-prev':
            cursor = max(cursor - 1, 0)
        elif cmd == 'focus-node-beg':
            cursor = 0
        elif cmd == 'focus-node-end':
            cursor = len(entries) - 1

    if cmd == 'shift-node-parent':
        shift('left')
    elif cmd == 'shift-node-current':
        shift('right')


def update(cmd):
    if cmd == 'quit':
        return True
    with threading.Lock():
        docmd(cmd)
