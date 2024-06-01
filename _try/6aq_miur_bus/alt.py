#!/usr/bin/python3
#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#

import sys
import logging

import miur.graph as M


if __name__ == '__main__':
    logging.basicConfig(
        level=logging.DEBUG,
        filename='/tmp/miur.log',
        datefmt='%H:%M:%S',
        format=("%(asctime)s %(name)8s %(levelname)s " +
                "[%(module)s:%(lineno)d]: %(message)s")
    )

    M.run(sys.argv)
