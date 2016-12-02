#!/usr/bin/python

import time
import logging
import multiprocessing as mp

from miur.ui import client

server_address = ('127.0.0.1', 8888)


# DEV: redirect stderr to logging
# SEE http://stackoverflow.com/questions/7714868/python-multiprocessing-how-can-i-reliably-redirect-stdout-from-a-child-process
def core():
    from miur.core import server
    try:
        # Serve requests until Ctrl+C is pressed
        server.main_loop(server_address)
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    # Quiet poll
    # logging.getLogger('asyncio').setLevel(logging.WARNING)
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

    for p in prs:
        p.start()

    # BUG:WARN: wait until cursor server started
    #   => otherwise exception 'ConnectionRefusedError'
    time.sleep(0.5)

    try:
        from miur.ui import main
        main.main(server_address)
    except KeyboardInterrupt:
        pass

    for p in reversed(prs):
        try:
            p.join()
        except KeyboardInterrupt:
            pass

    logging.info("end")
