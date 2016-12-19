# RFC: pack/unpack will be the same for all streams/pipes/fifo
# BUT: for UDP unpack requires sending requests for repeating lost UDP packets
# DEV: periodically send 'heartbeat' data and drop incomplete msg on each
#   heartbeat, raising error or requesting msg re-send from client
# MAYBE protocol is exactly this functionality for merging segments and heartbeat ?
#     => then curr code in Protocol must be moved into Presentation
# ALT:(name): StreamProtocol

import struct

from miur.share import ifc

__all__ = ['Desegmentate', 'Segmentate']


class Desegmentate(ifc.Link):
    h_sz_len = 4

    def __init__(self):
        self._n = self.h_sz_len
        self._buf = bytearray()
        self._head = True

    def __call__(self, data):
        """ Accumulate data even if too small for both branches """
        self._buf = self._parse(self._buf + data)

    # NOTE: used single cycle to process multiple msgs received at once
    # ALT: can only be implemented with generators BUT: how to be in C++ ?
    def _parse(self, buf):
        i = 0
        while (i + self._n) <= len(buf):
            blob = buf[i:i + self._n]
            i += self._n
            if self._head:
                self._n = struct.unpack('>I', blob)[0]
            else:
                self._n = self.h_sz_len
                self._slot(blob)
            self._head = not self._head
        return buf[i:]


# NOTE: TCP.transport.write() supports arbitrary data length BUT may be necessary for UDP
class Segmentate(ifc.Link):
    def __call__(self, data):
        header = struct.pack('>I', len(data))
        self._slot(header + data)
