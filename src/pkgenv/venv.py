import os
import os.path as fs
import sys


# TRY:OPT: do same for miur inof re-exec (PERF: for faster startup)
class ensure_venv:
    def __init__(self, venv: str) -> None:
        self._venv = venv
        self._shpath: str
        self._pypath: list[str]

    def __enter__(self) -> str:
        sys.prefix = sys.exec_prefix = venv = self._venv
        self._shpath = os.environ["PATH"]
        os.environ["PATH"] = fs.join(venv, "bin") + os.pathsep + os.environ["PATH"]
        self._pypath = sys.path
        sys.path = [p for p in sys.path if not p.endswith("site-packages")]
        pyxy = f"python{(ver:=sys.version_info)[0]}.{ver[1]}"
        # NOTE: same as how "site.py" changes sys.prefix
        # OR:BET? site.addsitepackages(sys.path, [sys.prefix])
        #   OR: if sys.flags.isolated: __import__("site").main()  # lazy init for "site" in isolated mode
        #   BAD: slow query for site()
        sys.path.append(fs.join(venv, sys.platlibdir, pyxy, "site-packages"))
        # print(sys.path)
        return venv

    def __exit__(self, _et, exc, _tb):  # type:ignore[no-untyped-def]
        os.environ["PATH"] = self._shpath
        sys.path = self._pypath
        sys.prefix = sys.base_prefix
        sys.exec_prefix = sys.base_exec_prefix
