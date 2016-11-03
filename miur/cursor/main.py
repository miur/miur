import logging

from miur.relay.eventdriver import EventDriver


def hello(msg):
    logging.info(msg)


# return os.listdir(p)
def main(server_address):
    with EventDriver(server_address, hello) as loop:
        try:
            loop.run_forever()
        except KeyboardInterrupt:
            pass
