#!/usr/bin/python

import time
import logging
import multiprocessing as mp

import miur.core.main as core
import miur.ui.main as ui

server_address = ('127.0.0.1', 8888)


# DEV: redirect stderr to logging
# SEE http://stackoverflow.com/questions/7714868/python-multiprocessing-how-can-i-reliably-redirect-stdout-from-a-child-process
def setup_log():
    # Quiet poll
    # logging.getLogger('asyncio').setLevel(logging.WARNING)
    logging.basicConfig(level=logging.DEBUG,
                        filename='/tmp/miur.log',
                        datefmt='%H:%M:%S',
                        format="%(asctime)s %(name)8s %(levelname)s " +
                        "[%(module)s:%(lineno)d]: %(message)s")


if __name__ == '__main__':
    setup_log()
    logging.info('*' * 50)
    prs = []
    # mp.set_start_method('spawn')
    # exit_event = mp.Event()

    # EXPL: pass args to process
    prs.append(mp.Process(target=core.main, args=(server_address,)))

    for p in prs:
        p.start()

    # BUG:WARN: wait until cursor server started
    #   => otherwise exception 'ConnectionRefusedError'
    time.sleep(0.5)
    ui.main(server_address)

    for p in reversed(prs):
        try:
            p.join()
        except KeyboardInterrupt:
            pass

    logging.info("end")
