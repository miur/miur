import threading

entries = []


def update(data):
    global entries

    # threading.current_thread()
    with threading.Lock():
        entries += data
