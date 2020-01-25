#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#

import zmq
from zmq import devices


def broker_hub(src_uri, dst_uri):
    hub = devices.ThreadDevice(zmq.FORWARDER, zmq.PULL, zmq.XPUB)
    hub.bind_in(dst_uri)
    hub.bind_out(src_uri)
    hub.start()
