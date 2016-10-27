import os
import threading

# MOVE: global var to local registry hive
entries = []


def update(data):
    global entries

    # threading.current_thread()
    with threading.Lock():
        entries = os.listdir(data)
        # entries += data
