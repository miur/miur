#!/usr/bin/python

from miur.ui import tui
# from miur.relay.server import Listener


if __name__ == '__main__':
    # BAD: not deleted afterwards
    # with Listener('/tmp/miur') as l:
    # with Listener(("localhost", 7773)) as l:
    #     tui.main(l.server.server_address)
    tui.main(None)
