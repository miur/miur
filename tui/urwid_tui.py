#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#

import pickle
import urwid
import zmq

from ..ifc import *


def tui_client(src_uri, dst_uri, log_uri):
    set_current_thread_name()
    _log = getLogger()

    ctx = zmq.Context.instance()

    # NOTE:(cohesion): connect topology backward :: from dst to src
    dst_sock = ctx.socket(zmq.PUSH)
    dst_sock.connect(dst_uri)

    src_sock = ctx.socket(zmq.SUB)
    src_sock.connect(src_uri)
    src_sock.setsockopt_string(zmq.SUBSCRIBE, '*')  # custom broadcast
    src_sock.setsockopt_string(zmq.SUBSCRIBE, 'ui')

    try:
        ## BET: python-urwid
        # [Urwid] key capture in different views
        # http://lists.excess.org/pipermail/urwid/2011-July/001080.html
        body = urwid.Text("<Press ',' to exit>")
        view = urwid.Filler(body, 'top')

        def unhandled_input(key):
            if key in ('esc', ','):
                raise urwid.ExitMainLoop()
            dst_sock.send_multipart([b'key', pickle.dumps(key)])
            _log.info("Press: " + key)

            # FIXME: change text only in subscriber
            body.set_text(repr(key))

        loop = urwid.MainLoop(view, unhandled_input=unhandled_input)
        loop.run()

    except KeyboardInterrupt:
        pass
    finally:
        # _log.info("Fin: " + threading.current_thread().name)
        # dst_sock.send_multipart([b'*', pickle.dumps('quit')])
        dst_sock.close()
        src_sock.close()
        # ERR:(exception): can call only once in main thread
        # zmq.Context.instance().term()
