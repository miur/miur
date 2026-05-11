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


# TODO: Pre-fetching: If your user is scrolling down, you can use os.posix_fadvise (on Linux)
#  with POSIX_FADV_SEQUENTIAL to tell the kernel to keep the pages
#  just ahead of the current scroll position ready in the page cache.
# * On Jump: Use POSIX_FADV_WILLNEED to tell the OS to start loading the jump target into the page cache immediately.
# * On Scroll: Use POSIX_FADV_SEQUENTIAL (for forward) or POSIX_FADV_WILLNEED (for backward) on the next ~64KB chunk.
# * On "Leave": When switching files, use POSIX_FADV_DONTNEED to free up the physical RAM, keeping only the virtual map.

import mmap
import os


def manage_scrolling(file_path, offset, window_size=10240):
    with open(file_path, "rb") as f:
        fd = f.fileno()
        # Initial map is instant
        mm = mmap.mmap(fd, 0, access=mmap.ACCESS_READ)

        # 1. THE RANDOM JUMP: Tell OS to prep a specific region
        # We hint a bit more than the 10kB window to handle immediate micro-scrolls
        hint_size = window_size * 5
        os.posix_fadvise(fd, offset, hint_size, os.POSIX_FADV_WILLNEED)

        # 2. THE RENDER: This will now likely hit RAM, not Disk
        view = mm[offset : offset + window_size]

        # 3. THE PRE-FETCH (Predictive)
        # If user is scrolling down, hint the next chunk
        next_chunk_offset = offset + window_size
        os.posix_fadvise(fd, next_chunk_offset, hint_size, os.POSIX_FADV_SEQUENTIAL)

        # 4. THE BACK-SCROLL (Manual hint)
        # If user is near the top of the view, hint the previous chunk
        if offset > hint_size:
            prev_chunk_offset = offset - hint_size
            os.posix_fadvise(fd, prev_chunk_offset, hint_size, os.POSIX_FADV_WILLNEED)

        return view


# Since you are switching between hundreds of files, you will eventually hit the vm.max_map_count limit or the
# open file limit. When you switch away from a file, explicitly call:
# os.posix_fadvise(fd, 0, 0, os.POSIX_FADV_DONTNEED) # 0, 0 means "whole file"


import mmap
import os
from collections import OrderedDict


class FileViewManager:
    # INFO Open Files: ulimit -n (Default is often 1024). Keep your cache_size safely below this.
    # INFO Memory Maps: cat /proc/sys/vm/max_map_count (Default is usually 65530). This is usually plenty for hundreds of files.
    def __init__(self, cache_size=100, small_file_threshold=4 * 1024 * 1024):
        self.cache = OrderedDict()  # Stores {path: (mmap_obj, file_handle)}
        self.small_files = {}  # Stores {path: bytes} for tiny files
        self.cache_size = cache_size
        self.threshold = small_file_threshold

    def get_view(self, path, offset, window=10240):
        # 1. Check if it's a known small file
        if path in self.small_files:
            return self.small_files[path][offset : offset + window]

        # 2. Check if it's already in the mmap cache
        if path in self.cache:
            self.cache.move_to_end(path)  # Mark as recently used
            mm, _ = self.cache[path]
            self._hint_os(path, offset, window)
            return mm[offset : offset + window]

        # 3. Load new file
        file_size = os.path.getsize(path)

        # Strategy A: Read Whole (Small Files)
        if file_size < self.threshold:
            with open(path, "rb") as f:
                data = f.read()
                self.small_files[path] = data
                return data[offset : offset + window]

        # Strategy B: mmap (Large Files)
        if len(self.cache) >= self.cache_size:
            self._evict_oldest()

        f = open(path, "rb")
        mm = mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ)
        self.cache[path] = (mm, f)

        self._hint_os(path, offset, window)
        return mm[offset : offset + window]

    def _hint_os(self, path, offset, window):
        # Predictive hinting for bidirectional scrolling
        fd = self.cache[path][1].fileno()
        # Hint current + next 64KB for smooth forward scroll
        os.posix_fadvise(fd, offset, window * 6, os.POSIX_FADV_WILLNEED)
        # Hint previous 64KB for smooth backward scroll
        if offset > 65536:
            os.posix_fadvise(fd, offset - 65536, 65536, os.POSIX_FADV_WILLNEED)

    def _evict_oldest(self):
        path, (mm, f) = self.cache.popitem(last=False)
        # Final hint to OS: "We don't need this in RAM anymore"
        os.posix_fadvise(f.fileno(), 0, 0, os.POSIX_FADV_DONTNEED)
        mm.close()
        f.close()
