import os
import os.path as fs
import sys


def vpip(*args: str, output: bool = False) -> str:
    from subprocess import run

    # OFF:API: https://pip.pypa.io/en/latest/user_guide/#using-pip-from-your-program
    # TUT: https://realpython.com/what-is-pip/
    pipa = (sys.executable, "-m", "pip", "--require-virtualenv", "--isolated")
    kw = {"capture_output": True, "text": True} if output else {}
    ret = run([*pipa, *args], check=True, **kw)  # type:ignore
    return ret.stdout if output else ""


# SRC: https://stackoverflow.com/questions/1158076/implement-touch-using-python
def touch(fname: str, dir_fd: int | None = None) -> None:
    flags = os.O_CREAT | os.O_APPEND
    with os.fdopen(os.open(fname, flags=flags, mode=0o666, dir_fd=dir_fd)) as f:
        os.utime(
            f.fileno() if os.utime in os.supports_fd else fname,
            dir_fd=None if os.supports_fd else dir_fd,
        )


# TODO: use ".venv_release" for primary "requirements.txt"
#     (and continue using ".venv" for "requirements_dev.txt")
#   COS: to verify we have all necessary parts present
#     BAD: we load most of modules lazily, so you need actual integration tests
def install_venv_deps(devroot: str | None = None, dev: bool = False) -> None:
    if devroot is None:
        # devroot = fs.dirname(fs.dirname(fs.realpath(sys.modules["__main__"].__file__)))
        devroot = "/d/miur"
    venvstamp = fs.join(devroot, ".venv/_updated")
    reqpfx = fs.join(devroot, "pkg/requirements")
    reqs = [(reqpfx + sfx + ".txt") for sfx in ("", "_dev")]

    # EXPL: don't upgrade .venv if all requirements.txt haven't changed
    if not dev and fs.exists(venvstamp):
        last = os.stat(venvstamp).st_mtime
        if os.stat(reqs[0]).st_mtime < last:
            if not dev or os.stat(reqs[1]).st_mtime < last:
                return

    vpip("install", "--upgrade", "pip")
    vpip("install", "--upgrade", "-r", (reqs[1] if dev else reqs[0]))

    ## NOTE: install !miur into venv site to be used as pkg by my other tools
    ## DISABLED:BAD: changes "hash" in reqs_all.txt after each install()
    # vpip("install", "--editable", devroot)

    ##%ONELINE: pip freeze > requirements_all.txt
    with open(reqpfx + "_frozen.txt", "w", encoding="utf-8") as f:
        f.write(vpip("freeze", output=True))

    # MAYBE: use frozen reqs as a stamp file ?
    #   BAD: on git-clone all files will have the same mtime
    touch(venvstamp)


# FIND: is there any standard way in latest python>=3.12 ?
def get_py_args(appargs: bool = True) -> list[str]:
    import ctypes

    argc = ctypes.c_int()
    argv = ctypes.POINTER(ctypes.c_wchar_p)()
    ctypes.pythonapi.Py_GetArgcArgv(ctypes.byref(argc), ctypes.byref(argv))
    num = argc.value if appargs else argc.value - len(sys.argv) + 1
    return [argv[i] for i in range(num)]


def ensure_venv(devroot: str) -> None:
    if sys.prefix == sys.base_prefix:
        import venv

        # SRC: https://stackoverflow.com/questions/6943208/activate-a-virtualenv-with-a-python-script/77635818#77635818
        vpath = fs.join(devroot, ".venv")
        if not fs.exists(vpath):
            # OFF: https://docs.python.org/3/library/venv.html
            venv.create(vpath, with_pip=True)

        ## DISABLED:BAD: interferes with .py apps launched from nested shell
        # os.environ["VIRTUAL_ENV"] = vpath
        # os.environ["PATH"] = vpath + ":" + os.environ["PATH"]

        vexe = fs.join(vpath, "bin/python")

        cmd = get_py_args()
        if vexe == cmd[0]:
            exc = RuntimeError("ERR: endless loop in exec(.venv)")
            exc.add_note(" * manually REMOVE '-S' from python interp args")
            exc.add_note(f" * {cmd}")
            raise exc
        cmd[0] = vexe

        # HACK: start miur in isolated mode, but then drop it to be able to use .venv
        #   CHECK: is it a bug, that "-S" affects .venv ?
        if cmd[1] == "-S":
            del cmd[1]
        elif cmd[1].startswith("-"):
            cmd[1] = cmd[1].replace("S", "")

        os.execv(cmd[0], cmd)
    else:
        install_venv_deps(devroot, dev=False)
