#!/usr/bin/python

import time
import logging
import multiprocessing as mp

from miur.ui import client
from miur.cursor import message, update


server_address = ('127.0.0.1', 8888)


# DEV: redirect stderr to logging
# SEE http://stackoverflow.com/questions/7714868/python-multiprocessing-how-can-i-reliably-redirect-stdout-from-a-child-process
def core():
    from miur.core import main
    try:
        main.main(server_address)
    except KeyboardInterrupt:
        pass


def test(obj):
    try:
        time.sleep(0.5)
        client.send_once(server_address, obj)
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG,
                        filename='/tmp/miur.log',
                        datefmt='%H:%M:%S',
                        format="%(asctime)s %(levelname)s " +
                        "[%(module)s:%(lineno)d] %(message)s")
    logging.info('*' * 50)

    prs = []
    # mp.set_start_method('spawn')
    # exit_event = mp.Event()

    # EXPL: pass args to process
    # p_curs = mp.Process(target=cursor, args=(server_address,))
    prs.append(mp.Process(target=core))
    # prs.append(mp.Process(target=test, args=('test',))

    for p in prs:
        p.start()

    try:
        from miur.ui import tui
        # BUG:WARN: wait until cursor server started
        #   => otherwise exception 'ConnectionRefusedError'
        time.sleep(0.5)
        prs.append(client.run_in_background(server_address))
        tui.main(None)
    except KeyboardInterrupt:
        pass

    # DEV: send 'quit all' through socket
    logging.info("exiting")
    # BAD: client able to exit only if response come -- and hangs otherwise
    #   => last msg is empty '' on connection lost -> can't be used to set
    #   'is_watching=False' inside cmd.Quit()
    client.is_watching = False
    update.handle(message.QuitMsg())

    for p in reversed(prs):
        try:
            p.join()
        except KeyboardInterrupt:
            pass

    logging.info("end")
