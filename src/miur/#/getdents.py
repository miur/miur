import ctypes
import os

# Syscall number for x86_64
SYS_GETDENTS64 = 217
libc = ctypes.CDLL("libc.so.6", use_errno=True)


def list_dir_ultra_lean(path):
    fd = os.open(path, os.O_RDONLY | os.O_DIRECTORY)

    # 1MB buffer: Large enough for thousands of entries per syscall
    buf_size = 1024 * 1024
    buf = bytearray(buf_size)

    # Use memoryview to avoid copying the bytearray during slicing
    mv = memoryview(buf)

    try:
        while True:
            # Fill the buffer via syscall
            # We pass the address of the underlying buffer to ctypes
            nbytes = libc.syscall(
                SYS_GETDENTS64,
                fd,
                (ctypes.c_char * buf_size).from_buffer(buf),
                buf_size,
            )

            if nbytes <= 0:
                break

            ptr = 0
            while ptr < nbytes:
                # 1. Read record length (offset 16, 2 bytes)
                d_reclen = int.from_bytes(mv[ptr + 16 : ptr + 18], "little")

                # 2. Identify file type (offset 18, 1 byte)
                d_type = mv[ptr + 18]

                # 3. Get the name without creating a string yet
                # Filename starts at offset 19 and is null-terminated
                name_start = ptr + 19
                # Find the first \0 after the name_start within the record
                name_end = buf.find(b"\x00", name_start, ptr + d_reclen)

                # ONLY create a Python object if it's not . or ..
                raw_name = mv[name_start:name_end]
                if raw_name != b"." and raw_name != b"..":
                    # Yield the raw bytes to avoid Unicode conversion overhead
                    # or decode only if necessary: raw_name.tobytes().decode()
                    yield raw_name, d_type

                ptr += d_reclen
    finally:
        os.close(fd)


# Usage Example
for name_bytes, ftype in list_dir_ultra_lean("/usr/bin"):
    # name_bytes is a memoryview slice; very cheap!
    if name_bytes.startswith(b"python"):
        print(f"Found: {name_bytes.tobytes().decode()} Type: {ftype}")
