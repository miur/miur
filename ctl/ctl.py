#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#

import threading

import zmq


def create_instance(argv):
    src_uri = "inproc://broker"     # RENAME: events
    dst_uri = "inproc://feedback"   # RENAME: posts
    log_uri = "inproc://log"
    trace_uri = "inproc://trace"
    client_uri = (src_uri, dst_uri, log_uri)

    # WARN: can call only once in main thread
    # NEED: call before joining threads to interrupt zmq.proxy
    zmq.Context.instance().term()

    for t in threading.enumerate():
        if t is not threading.main_thread():
            t.join()
