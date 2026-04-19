#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#

import sys, os, os.path as fs
import logging
import pickle

import zmq

from ..ifc import *

def getLogger():
    fmt = ' '.join([
        "%(asctime)s",
        "%(process:d)/%(processName:s)",
        "%(thread:d)/%(threadName:s)",
        "[%(name:s)/%(levelname:s)]",
        "%(message)s",
        ":/%(pathname)s:%(lineno)d",
    ])

    # ALT: use logging.dictConfig()
    # TUT: Configuring Python logging for a local module differently to the main file - Stack Overflow ⌇xN(nu
    #     https://stackoverflow.com/questions/51721895/configuring-python-logging-for-a-local-module-differently-to-the-main-file
    # SEE: https://stackoverflow.com/questions/38323810/does-pythons-logging-config-dictconfig-apply-the-loggers-configuration-setti
    logging.basicConfig(
        level=logging.DEBUG,
        # handlers=(handler,),
        datefmt="%H:%M:%S.uuu",
        format=fmt
    )
    _log = logging.getLogger(__name__.split('.', 2)[1])
    _log.propagate = False
    return _log


# def logger_init():
#     # TODO: setup new FileHandler instance and redirect everything there
#     #   FAIL: must forward all formatter columns as structured log here
#     return lambda x: print(x, file=sys.stderr)
#     # SEE: https://www.datadoghq.com/blog/python-logging-best-practices/
#     # TRY: directly specify logging.dictConfig() with global handlers
#     _log = getLogger()
#     # _log = redirect_log(handler, lvl)
#     # _log = filename_log()
#     _log.info('-' * 40)


def logger_events(src_uri, dst_uri, log_uri):
    set_current_thread_name()
    # _log = getLogger()

    ctx = zmq.Context.instance()

    src_sock = ctx.socket(zmq.SUB)
    src_sock.connect(src_uri)
    src_sock.setsockopt_string(zmq.SUBSCRIBE, '')

    try:
        while True:
            event = src_sock.recv_string()
            lvl = logging.DEBUG
            # _log.info('event: ' + event)
            # msg = pickle.loads(frames[1])
            # _log.log(lvl, '[' + topic + '] ' + str(msg))

    except (zmq.ContextTerminated, KeyboardInterrupt):
        pass
    except:
        try:
            # ALT:MOVE: threading.excepthook = handle_exception  # NEED: python>=3.8
            # IDEA: gather all such code inside :/ctl/ lambda instantiation per thread
            dst_sock = ctx.socket(zmq.PUSH)
            dst_sock.connect(dst_uri)
            dst_sock.send_multipart([b'logger', 'exception'])
            dst_sock.close()
        except:
            pass
    finally:
        src_sock.close()
        # _log.info('exit')


def logger_sink(src_uri, dst_uri, log_uri):
    set_current_thread_name()
    # _log = getLogger()

    ctx = zmq.Context.instance()

    # ATT: using XSUB as sink has no benefits over SUB, only pains:
    #   => you must send customly composed sub msg to it to activate
    log_sock = ctx.socket(zmq.SUB)
    log_sock.bind(log_uri)

    # HACK: subscribe to only specific log-level '' => 'INFO'
    #   BAD: useless without comparison 'lvl>INFO'
    #   BET: use <data-channel> name as first topic for filtering to make easier
    #     communication pipeline deciphering
    log_sock.setsockopt_string(zmq.SUBSCRIBE, '')

    try:
        while True:
            log = log_sock.recv_multipart()
            # _log.info('log: ' + str(log))

            # frames = sub.recv_multipart()
            # topic = frames[0].decode('utf-8')
            # lvl = getattr(logging, topic.partition('.')[2])
            # msg = frames[1].decode('utf-8')
            # if msg.endswith('\n'):
            #     msg = msg[:-1]

            # _log.log(lvl, '[' + topic + '] ' + str(msg))

    except (zmq.ContextTerminated, KeyboardInterrupt):
        pass
    except:
        try:
            # ALT:MOVE: threading.excepthook = handle_exception  # NEED: python>=3.8
            dst_sock = ctx.socket(zmq.PUSH)
            dst_sock.connect(dst_uri)
            dst_sock.send_multipart([b'logger', 'exception'])
            dst_sock.close()
        except:
            pass
    finally:
        log_sock.close()
        # _log.info('exit')
