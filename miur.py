#!/usr/bin/python

import time
import logging
import multiprocessing as mp

from miur.cursor import main as curs
from miur.ui import tui
# from miur.relay.server import Listener
from miur.relay.client import send

logging.basicConfig(filename='/tmp/miur.log', level=logging.DEBUG)


def test(server_address):
    try:
        time.sleep(1.0)
        send(server_address, 'hi')
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    # BAD: not deleted afterwards
    # with Listener('/tmp/miur') as l:
    # with Listener(("localhost", 7773)) as l:
    #     tui.main(l.server.server_address)

    # mp.set_start_method('spawn')

    # exit_event = mp.Event()
    server_address = ('127.0.0.1', 8888)
    # DEV: redirect stderr to logging
    # SEE http://stackoverflow.com/questions/7714868/python-multiprocessing-how-can-i-reliably-redirect-stdout-from-a-child-process
    p_curs = mp.Process(target=curs.main, args=(server_address,))
    p_test = mp.Process(target=test, args=(server_address,))

    p_curs.start()
    p_test.start()

    try:
        tui.main(None)
    except KeyboardInterrupt:
        logging.info("end")
        # shutdown to all
    finally:
        p_test.join()
        p_curs.join()
