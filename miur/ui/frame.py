import os
import threading

from miur.relay import client

# Server addr
saddr = None

# CHG: instead of predefined list
#   * input path from user
#   * react to keypress
dirs = [os.path.expanduser('~'), '/tmp', '/']

entries = []


def update(data):
    global entries

    with threading.Lock():
        entries = client.send(saddr, data)
        print(entries)
