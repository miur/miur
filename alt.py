#!/usr/bin/python3

import sys
import logging

import miur.alt as M


if __name__ == '__main__':
    logging.basicConfig(
        level=logging.DEBUG,
        filename='/tmp/miur.log',
        datefmt='%H:%M:%S',
        format=("%(asctime)s %(name)8s %(levelname)s " +
                "[%(module)s:%(lineno)d]: %(message)s")
    )

    M.run(sys.argv)
