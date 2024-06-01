#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
# USAGE: call once from main() of each application to register ZMQ log handler
#
__all__ = [
    'setup_logging',
]

import logging

import zmq
from zmq.log import handlers


def setup_logging(log_uri, lvl=None):
    # BUG: PUBHandler('inproc://log') will bind to address instead of connecting
    # FAIL: it creates additional two threads :: ZMQbg/IO/0 and ZMQbg/Reaper
    # handler = handlers.PUBHandler(log_uri)

    ctx = zmq.Context.instance()
    log_sock = ctx.socket(zmq.PUB)
    log_sock.connect(log_uri)

    handler = handlers.PUBHandler(log_sock)
    handler.root_topic = ''  # cmpt OR %(process:d)/%(processName:s)

    fmt = ' '.join([
        "%(asctime)s",
        "%(process:d)/%(processName:s)",
        "%(thread:d)/%(threadName:s)",
        "[%(name:s)/%(levelname:s)]",
        "%(message)s",
        ":/%(pathname)s:%(lineno)d",
    ])

    # ALT: use logging.dictConfig()
    # SEE: https://stackoverflow.com/questions/38323810/does-pythons-logging-config-dictconfig-apply-the-loggers-configuration-setti
    logging.basicConfig(
        level=(lvl if lvl is not None else logging.DEBUG),
        handlers=(handler,),
        datefmt="%H:%M:%S.uuu",
        format=fmt
    )

    return log_sock
