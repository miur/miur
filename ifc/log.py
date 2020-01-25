#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
__all__ = [
    'getLogger',
]

import logging
import sys

from zmq.log import handlers


## Gather all logs
# https://github.com/zeromq/pyzmq/blob/master/examples/logger/zmqlogger.py
# https://pyzmq.readthedocs.io/en/latest/api/zmq.log.handlers.html
# ++ https://stackoverflow.com/questions/40218325/use-of-pyzmqs-logging-handler-in-python
# [_] TRY: also redirect all logging to LTTng trace
def getLogger(pub_sock='inproc://log', lvl=None):
    # pid = os.getpid()
    # tid = threading.get_native_id()
    mod = sys._getframe(1).f_back.f_globals['__name__']
    cmpt = mod.partition('.')[2].partition('.')[0]
    cmpt = cmpt if cmpt else 'root'

    handler = handlers.PUBHandler(pub_sock)
    handler.root_topic = 'log'

    if lvl is None:
        lvl = logging.DEBUG
    handler.setLevel(lvl)

    # ALSO: %(processName)s %(threadName)s
    fmt = "%(asctime)s %(process:d) %(thread:d) [%(name:s)/%(levelname:s)] %(message)s :/%(pathname)s:%(lineno)d"
    handler.setFormatter(logging.Formatter(fmt, datefmt="%H:%M:%S"))

    # SEE: https://stackoverflow.com/questions/38323810/does-pythons-logging-config-dictconfig-apply-the-loggers-configuration-setti
    logger = logging.getLogger(mod)
    logger.addHandler(handler)
    return logger
