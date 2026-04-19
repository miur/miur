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
def install_venv_deps(devroot: str, dev: bool = False) -> None:
    venvstamp = fs.join(devroot, ".venv/_updated")
    reqpfx = fs.join(devroot, "pkg/requirements")
    usrtxt = reqpfx + ".txt"
    devtxt = reqpfx + "_dev.txt"

    # EXPL: don't upgrade .venv if all requirements.txt haven't changed
    if fs.exists(venvstamp):
        import hashlib

        with open(venvstamp, "r", encoding="utf-8") as f:
            stamp = f.read()
        datsz = int(nsz) if (nsz := stamp.partition(" ")[0]).isdigit() else -1

        with open(usrtxt, "r", encoding="utf-8") as f:
            # NOTE: calc and cmp SHA over sorted pkglist w/o comments
            usrdat = "".join(
                sorted(
                    k + os.linesep
                    for l in f
                    if (k := l.strip()) and not k.startswith("#")
                )
            ).encode(f.encoding)

        # NOTE: it's always calculated and compared when req.txt is the same,
        #   >> no sense to hide it deeper under if-else for when req.txt had changed
        usrhash = hashlib.sha256(usrdat).hexdigest()
        usrstamp = f"{len(usrdat)} {usrhash}"
        if datsz == len(usrdat):
            # THINK: combine and hash together with usrtxt ?
            # OPT: if not dev or ...
            venvts = os.stat(venvstamp).st_mtime
            devts = os.stat(devtxt).st_mtime
            if devts <= venvts:
                if stamp == usrstamp:
                    return

    # ALT: cat requirements.txt | /d/miur/.venv/bin/python -m pip install -r /dev/stdin
    #   &why: I can combine "usr" and "dev" and feed it as a single list at once
    #     NICE: able to remove "-r requirements.txt" from devtxt, and reuse it in pyproject.toml
    vpip("install", "--upgrade", "pip", "-r", (devtxt if dev else usrtxt))

    ## NOTE: install !miur into venv site to be used as pkg by my other tools
    ## DISABLED:BAD: changes "hash" in reqs_all.txt after each install()
    # vpip("install", "--editable", devroot)

    # ALT:MAYBE: use frozen reqs as a stamp file ?
    #   BAD: on git-clone all files will have the same mtime
    # touch(venvstamp)
    with open(venvstamp, "w", encoding="utf-8") as f:
        f.write(usrstamp)

    ##%ONELINE: pip freeze > requirements_frozen.txt
    # %USAGE:(cleanup after experimental deps):
    # %  $ ./.venv/bin/python -m pip uninstall -y -r =(./.venv/bin/python -m pip freeze | grep -vxFf ./pkg/requirements_frozen.txt)
    # %ALSO:(kill unnecessary instances of tools): $ pkill -f pylint
    # RENAME? "requirements.lock"
    # REMOVE? useless, as pip-sync should eliminate everything manually installed in .venv
    with open(reqpfx + "_frozen.txt", "w", encoding="utf-8") as f:
        # OR: pip list --format=freeze
        # MAYBE pip freeze --exclude-editable
        f.write(vpip("freeze", output=True))


# FIND: is there any standard way in latest python>=3.12 ?
def get_py_args(appargs: bool = True) -> list[str]:
    import ctypes

    argc = ctypes.c_int()
    argv = ctypes.POINTER(ctypes.c_wchar_p)()
    ctypes.pythonapi.Py_GetArgcArgv(ctypes.byref(argc), ctypes.byref(argv))
    num = argc.value if appargs else argc.value - len(sys.argv) + 1
    return [argv[i] for i in range(num)]


# TODO:CHG: "dev:bool" -> "optdeps:str=(dev,demo,opt)" (corresponding to pyproject.toml)
def ensure_venv(devroot: str, dev: bool = False) -> None:
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
        install_venv_deps(devroot, dev=dev)
