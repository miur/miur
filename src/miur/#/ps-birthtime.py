import ctypes
import os


# 1. Define Linux clock_gettime structures
class Timespec(ctypes.Structure):
    _fields_ = [("tv_sec", ctypes.c_long), ("tv_nsec", ctypes.c_long)]

libc = ctypes.CDLL("libc.so.6")
# CLOCK_MONOTONIC is standard identifier 1 on Linux
libc.clock_gettime.argtypes = [ctypes.c_int, ctypes.c_void_p]

def get_linux_process_age_seconds() -> float:
    # A. Get the system's clock ticks per second (usually 100)
    clk_tck = os.sysconf(os.sysconf_names["SC_CLK_TCK"])

    # B. Read the 22nd field (index 21) from /proc/self/stat
    with open("/proc/self/stat", "r") as f:
        stat_fields = f.read().split()
    start_time_ticks = int(stat_fields[21])

    # C. Convert ticks to seconds since boot
    process_boot_timestamp = start_time_ticks / clk_tck

    # D. Get current system uptime using CLOCK_MONOTONIC hardware clock
    ts = Timespec()
    libc.clock_gettime(1, ctypes.byref(ts))
    current_system_uptime = ts.tv_sec + (ts.tv_nsec / 1e9)

    # E. Delta yields the absolute lifespan of the OS process
    return current_system_uptime - process_boot_timestamp

# --- Usage Example ---
print(f"Total process life (inc. VM setup): {get_linux_process_age_seconds():.6f}s")
