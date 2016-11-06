#!/usr/bin/python

import logging
import multiprocessing as mp


server_address = ('127.0.0.1', 8888)


# DEV: redirect stderr to logging
# SEE http://stackoverflow.com/questions/7714868/python-multiprocessing-how-can-i-reliably-redirect-stdout-from-a-child-process
def cursor():
    from miur.cursor import main as curs
    try:
        curs.main(server_address)
    except KeyboardInterrupt:
        pass


def test():
    import time
    from miur.relay import aux
    try:
        time.sleep(0.5)
        # aux.send_once(server_address, 'test')
        aux.loop(server_address)
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO,
                        filename='/tmp/miur.log',
                        datefmt='%H:%M:%S',
                        format="%(asctime)s %(levelname)s " +
                        "[%(module)s:%(lineno)d] %(message)s")
    logging.info('*' * 50)

    # mp.set_start_method('spawn')
    # exit_event = mp.Event()

    # EXPL: pass args to process
    # p_curs = mp.Process(target=cursor, args=(server_address,))
    p_curs = mp.Process(target=cursor)
    p_test = mp.Process(target=test)

    p_curs.start()
    p_test.start()

    try:
        from miur.ui import tui
        # tui.main(None)
    except KeyboardInterrupt:
        pass

    # DEV: send 'quit all' through socket

    try:
        p_test.join()
    except KeyboardInterrupt:
        pass

    try:
        p_curs.join()
    except KeyboardInterrupt:
        pass

    logging.info("end")
