# BAD: broken chain of ret= value when accumulating
#   ~? ret 'None' when consumed and 'car' when fully converted
# ALT: can only be implemented with generators
#   BUT: how to be in C++ ?

import struct

__all__ = ['Desegmentate', 'Segmentate']


class Desegmentate:
    h_sz_len = 4

    def __init__(self):
        self._n = self.h_sz_len
        self._buf = bytearray()
        self._head = True

    def __call__(self, data):
        """ Accumulate data even if too small for both branches """
        buf = self._buf + data
        i = 0
        # NOTE: used single cycle to process multiple msgs received at once
        while (i + self._n) <= len(buf):
            blob = buf[i:i + self._n]
            i += self._n
            if self._head:
                self._n = struct.unpack('>I', blob)[0]
            else:
                self._n = self.h_sz_len
                ret = yield blob
            self._head = not self._head
        self._buf = buf[i:]
        return ret


# NOTE: TCP.transport.write() supports arbitrary data length BUT may be necessary for UDP
class Segmentate:
    def __call__(self, data):
        header = struct.pack('>I', len(data))
        return header + data
