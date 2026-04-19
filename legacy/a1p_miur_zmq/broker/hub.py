#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#

import zmq
from zmq import devices

from ..ifc import *

def broker_thread(src_uri, dst_uri, log_uri):
    set_current_thread_name()

    ## FAIL:(no blocking): load 100% CPU due to polling
    hub = devices.Device(zmq.FORWARDER, zmq.PULL, zmq.XPUB)
    hub.bind_in(dst_uri)
    hub.bind_out(src_uri)
    # hub.bind_mon(log_uri)
    hub.start()


def broker_hub(src_uri, dst_uri, log_uri):
    set_current_thread_name()

    ctx = zmq.Context.instance()

    # NOTE: hub has inversed direction of message passing :: dst => src
    dst_sock = ctx.socket(zmq.XPUB)
    dst_sock.bind(src_uri)
    src_sock = ctx.socket(zmq.PULL)
    src_sock.bind(dst_uri)

    try:
        while True:
            frames = src_sock.recv_multipart()
            dst_sock.send_multipart(frames)

    except (zmq.ContextTerminated, KeyboardInterrupt):
        pass
    except:
        try:
            dst_sock.send_multipart([b'hub', 'exception'])
        except:
            pass
    finally:
        src_sock.close()
        dst_sock.close()
        # log_sock.info('exit')
