# ALT(inotify): root system-wide monitoring
#   https://pypi.org/project/pyfanotify/
#   https://pypi.org/project/butter/
#     https://github.com/google/python-fanotify
#   * auditctl -- to also detect user/process who modified the file

from watchdog.events import FileSystemEventHandler
from watchdog.observers import Observer


class RecreateHandler(FileSystemEventHandler):
    def __init__(self, manager, callback):
        self.manager = manager
        self.callback = callback

    def on_modified(self, event):
        if event.src_path == self.manager.path:
            self.callback()

    def on_created(self, event):
        if event.src_path == self.manager.path:
            self.manager._setup_mmap()
            self.callback()


def start_viewer(path):
    manager = FileContentProxy("data.bin")

    def redraw():
        data = manager.safe_read(1024, 100)
        if data:
            print(f"Redraw UI: {data}")

    ## FIXME: If the kernel-level notifications fail (common on network filesystems like NFS or SMB),
    ##   you must fall back to standard polling. Regularly checking the file's modification time (mtime) using os.stat().
    ##   USE: Watchdog includes a built-in fallback implementation called PollingObserver
    # from watchdog.observers.polling import PollingObserver
    # observer = PollingObserver(timeout=1.0) # timeout is the poll interval in seconds
    observer = Observer()
    observer.schedule(RecreateHandler(manager, redraw), path, recursive=False)
    observer.start()
    try:
        while True:
            time.sleep(1)
    finally:
        observer.stop()
        observer.join()



import fcntl


def content_read_bytes_flock(self, h: HPath, offset: int, length: int) -> bytes:
    # MAYBE: with self._lock:
    try:
        ## ALT:(Unix + Windows): pip install portalocker
        # with portalocker.Lock(self.path, mode="rb", flags=portalocker.LOCK_SH | portalocker.LOCK_NB):
        # ... except portalocker.exceptions.LockException: return None
        ### WARN: On some networked filesystems it might be needed to force a os.fsync() before closing
        ###   the file so it’s actually written before another client reads the
        ## with portalocker.Lock('some_file', 'rb+', timeout=60) as fh:
        ##     fh.write(...); fh.flush(); os.fsync(fh.fileno())
        fcntl.flock(self._fd, fcntl.LOCK_SH | fcntl.LOCK_NB)
        # mapped_offset = offset - self._aligned_offset
        # ARCH:ATT: we are forced to call _ensure_mmap() in each function,
        #   because file may change externally and we may need remap
        # self._ensure_mmap()  # OR: if not self.mm: return None
        try:
            ### Prevent out-of-bounds reads
            ## WARN:MAYBE:(anyway): try: ... except ValueError: return None
            #   WHY: mmap will raise this if you attempt to access an offset that
            #     became invalid because another process truncated the file.
            # end = min(offset + length, self._mm.size())
            # if offset >= self._mm.size():
            #     return b""
            # return self._mm[mapped_offset : mapped_offset + length]
            return self.content_read_bytes(h, offset, length)
        finally:
            fcntl.flock(self._fd, fcntl.LOCK_UN)
    except BlockingIOError as exc:
        # ALT: return None
        exc.add_note("File is locked by an exclusive writer")
        raise exc from exc
