import curses
import logging

from . import client
from .loop import loop
from miur.cursor import message, update


def main(server_address):
    conn = client.run_in_background(server_address)

    try:
        # EXPL: Init first screen (WARN: multithreading timings)
        update.update('_init')
        curses.wrapper(loop)
    finally:
        # CHECK: if exceptions are re-raised

        # DEV: send 'quit all' through socket
        logging.info("exiting")
        # BAD: client able to exit only if response come -- and hangs otherwise
        #   => last msg is empty '' on connection lost -> can't be used to set
        #   'is_watching=False' inside cmd.Quit()
        client.is_watching = False
        try:
            update.handle(message.QuitMsg())
        finally:
            conn.join()
