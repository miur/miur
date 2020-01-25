#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#

import sys
import time

import zmq

from ..ifc import *


def play_scenario(src_uri, dst_uri, log_uri):
    set_current_thread_name()
    # _log = getLogger(log_uri)

    ctx = zmq.Context.instance()

    # NOTE:(cohesion): connect topology backward :: from dst to src
    dst_sock = ctx.socket(zmq.PUSH)
    dst_sock.connect(dst_uri)

    ## FUTURE: use for conditional switching of scenario
    # src_sock = ctx.socket(zmq.SUB)
    # src_sock.connect(src_uri)

    try:
        cmd = 0
        while cmd := cmd + 1:
            print('cmd: ' + str(cmd), file=sys.stderr)
            dst_sock.send_string(str(cmd))
            # _log.debug(str(cmd))
            time.sleep(0.25)

    except (zmq.ContextTerminated, KeyboardInterrupt):
        pass
    finally:
        dst_sock.close()
        # src_sock.close()
