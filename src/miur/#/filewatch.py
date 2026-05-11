# ALT(inotify): root system-wide monitoring
#   https://pypi.org/project/pyfanotify/
#   https://pypi.org/project/butter/
#     https://github.com/google/python-fanotify
#   * auditctl

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
