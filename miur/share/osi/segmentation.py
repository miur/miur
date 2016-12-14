# BAD: broken chain of ret= value when accumulating
#   ~? ret 'None' when consumed and 'car' when fully converted
# ALT: can only be implemented with generators
#   BUT: how to be in C++ ?

import struct

from .basechainlink import BaseChainLink

__all__ = ['Desegmentate', 'Segmentate']


class Desegmentate(BaseChainLink):
    h_sz_len = 4

    def __init__(self):
        self._n = self.h_sz_len
        self._buf = bytearray()
        self._head = True

    def __call__(self, data):
        """ Accumulate data even if too small for both branches """
        self._buf = self._parse(self._buf + data)

    def _parse(self, buf):
        i = 0
        # NOTE: used single cycle to process multiple msgs received at once
        while (i + self._n) <= len(buf):
            blob = buf[i:i + self._n]
            i += self._n
            if self._head:
                self._n = struct.unpack('>I', blob)[0]
            else:
                self._n = self.h_sz_len
                self.sink(blob)
            self._head = not self._head
        return buf[i:]


# NOTE: TCP.transport.write() supports arbitrary data length BUT may be necessary for UDP
class Segmentate(BaseChainLink):
    def __call__(self, data):
        header = struct.pack('>I', len(data))
        self.sink(header + data)
