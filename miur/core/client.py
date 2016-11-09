import logging
# import asyncio
import threading


_log = logging.getLogger(__name__)


class ClientsList(dict):
    def __init__(self):
        self._lock = threading.Lock()

    def __setitem__(self, id, obj):
        with self._lock:
            _log.info('Client {} memorized'.format(id))
            return super().__setitem__(id, obj)

    def __delitem__(self, id):
        # BAD: crash if waits on _lock until disconnectAll() exits
        with self._lock:
            _log.info('Client {} removed'.format(id))
            return super().__delitem__(id)

    def disconnectAll(self):
        # BAD: won't close any connection, waiting on _lock to add
        with self._lock:
            for client in self.values():
                _log.info('Closing the client {!r} socket'.format(client))
                client.transport.close()
