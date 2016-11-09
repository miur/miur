#!/usr/bin/python

import time
import logging
import multiprocessing as mp

from miur.relay import aux
from miur.graph import command as cmd


server_address = ('127.0.0.1', 8888)


# DEV: redirect stderr to logging
# SEE http://stackoverflow.com/questions/7714868/python-multiprocessing-how-can-i-reliably-redirect-stdout-from-a-child-process
def cursor():
    from miur.cursor import main as curs
    try:
        curs.main(server_address)
    except KeyboardInterrupt:
        pass


def test(obj):
    try:
        time.sleep(0.5)
        aux.send_once(server_address, obj)
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
    prs.append(mp.Process(target=cursor))
    # prs.append(mp.Process(target=test, args=('test',))

    for p in prs:
        p.start()

    try:
        from miur.ui import tui
        # BUG:WARN: wait until cursor server started
        #   => otherwise exception 'ConnectionRefusedError'
        time.sleep(0.5)
        prs.append(aux.run_in_background(server_address))
        tui.main(None)
    except KeyboardInterrupt:
        pass

    # DEV: send 'quit all' through socket
    aux.is_watching = False
    logging.info("exiting")
    aux.put_cmd_threadsafe(cmd.Quit())

    for p in reversed(prs):
        try:
            p.join()
        except KeyboardInterrupt:
            pass

    logging.info("end")
