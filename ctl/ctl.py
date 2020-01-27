#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#

import threading

import zmq

from ..ifc import *
from ..broker import broker_hub
from ..logger import setup_logging, logger_sink, logger_events
from ..scenario import play_scenario


def create_instance(opts):
    set_current_thread_name()

    src_uri = "inproc://broker"     # RENAME: events
    dst_uri = "inproc://feedback"   # RENAME: posts
    log_uri = "inproc://log"
    # trace_uri = "inproc://trace"

    connections = (src_uri, dst_uri, log_uri)

    setup_logging(log_uri)
    # threading.excepthook = handle_exception  # NEED: python>=3.8
    threading.Thread(target=broker_hub, args=connections).start()
    threading.Thread(target=logger_events, args=connections).start()
    threading.Thread(target=logger_sink, args=connections).start()

    # BET:TODO: set names of all processes/threads/asyncjobs from inside this ./ctl/
    play_scenario(*connections)

    # # BAD: "urwid" must be the main thread, because raw_screen.start() registers UNIX signals
    # ui_client(*connections)

    # WARN: can call only once in main thread
    # NEED: call before joining threads to interrupt zmq.proxy
    # DEV:TODO: staff each plugin into process/thread/asyncjob resource mgmt
    #   => conditionally auto-cleanup on exit depending on threading topology
    zmq.Context.instance().term()

    for t in threading.enumerate():
        if t is not threading.main_thread():
            t.join()
