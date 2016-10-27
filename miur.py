#!/usr/bin/python

from miur.ui import tui, frame
from miur.relay import client
from miur.relay.server import Listener


if __name__ == '__main__':
    # BAD: not deleted afterwards
    # with Listener('/tmp/miur') as l:
    with Listener(("localhost", 7773)) as l:
        saddr = l.server.server_address

        import os
        frame.entries = client.send(saddr, os.getcwd())
        frame.entries = client.send(saddr, '/tmp')

        tui.main()
