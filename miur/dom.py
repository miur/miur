import threading

import miur.access.fs as graph

# MOVE: global var to local registry hive
entries = []


def update(data):
    global entries

    # threading.current_thread()
    with threading.Lock():
        path = data
        entries = graph.list_nodes(path)
        # entries += data
