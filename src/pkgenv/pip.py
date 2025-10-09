import os
import os.path as fs
import subprocess as S
import sys


def sha_req() -> None:
    import importlib.metadata as md
    import json
    from urllib.request import urlopen

    vpkgs = md.distributions()
    for pk in vpkgs:
        wh = next(p for p in pk.files if str(p).endswith("/WHEEL"))
        tags = [
            l.removeprefix("Tag: ")
            for l in wh.read_text().splitlines()
            if l.startswith("Tag: ")
        ]
        with urlopen(f"https://pypi.org/pypi/{pk.name}/{pk.version}/json") as f:
            data = json.load(f)

        for u in data["urls"]:
            if any(
                u["filename"].startswith(f"{pk.name}-{pk.version}-{t}.") for t in tags
            ):
                sha = u["digests"]["sha256"]
                print(f"{pk.name}=={pk.version} --hash=sha256:{sha}")


def get_pip_user_cache_dir() -> str:
    if (d := os.environ.get("PIP_TOOLS_CACHE_DIR", "")).strip():
        return d
    if (d := os.environ.get("PIP_CACHE_DIR", "")).strip():
        return d
    ## WARN: it may also had been changed by pip.conf
    # $ pip config list -v
    # $ pip config --user set global.cache-dir /your/desired/path
    # $ pip config list
    # $ pip cache dir
    if not (c := os.environ.get("XDG_CACHE_HOME", "")).strip():
        # MAYBE: try-catch and fallback to /tmp or $PWD if no $HOME
        c = fs.expanduser("~/.cache")
    return c + "/pip"


def pin_requirements(
    tgt: str,
    aux: str,
    extras: str,
    args: list[str] | None = None,
) -> None:
    """WF: pypj.toml -> reqs.txt"""

    # [_] CHG: directly call "pip-compile" and expect PATH to be resolved to .venv
    #   WARN: when installed through "python3.13 -m pip install pip-tools" shebang is patched
    #     >> then "/home/user/.local/bin/pip-sync" has shebang "#!/usr/bin/python3.13"
    #     WTF: maybe os.environ["PATH"] is not inherited to override !python by venv ?
    #   TODO: disallow non-VENV calls by sys.prefix==sys.base_prefix ( like PIP_REQUIRE_VIRTUALENV)
    # [_] CHG: customize .venv to be allowed in /opt/venvmiur or /cache/venvmiur or :/_pfx/venv
    exe = ("pip-compile",)
    ## FAIL:DISABLED: initial empty .venv don't have pip-tools installed
    # if sys.prefix != sys.base_prefix:
    #     exe = (sys.prefix + "/bin/python", "-m", "piptools", "compile")
    # else:
    #     # BAD: it's fine if pip-tools are installed into different python, only PATH matters
    #     exe = (sys.executable, "-m", "piptools", "compile")
    dld = get_pip_user_cache_dir()
    src = "../../pyproject.toml"
    # aux = "dev-requirements.txt"
    # tgt = "requirements.txt"

    import hashlib
    import tomllib

    with open(src, "rb") as bf:
        toml = tomllib.load(bf)
    # groups = toml["dependency-groups"]
    deps = ": " + " ".join(sorted(toml["project"]["dependencies"]))
    optd = toml["project"]["optional-dependencies"]
    # BAD: too hard to calc hash only for specified "extras" due to its recursive refs (e.g. default=".[web]")
    #   &why PERF rebuild default reqs only when related list of deps changes
    deps += "\n".join(k + ": " + " ".join(sorted(optd[k])) for k in sorted(optd))
    dat = deps.encode("utf-8")
    h = hashlib.sha256(dat).hexdigest()
    stamp = f"# ser.input={src}:{len(dat)}:sha256:{h}\n".encode("utf-8")

    if fs.exists(tgt):
        # TEMP: unreliable
        # if fs.getmtime(tgt) > fs.getmtime(src):
        #     return
        with open(tgt, "rb") as f:
            if stamp == f.read(len(stamp)):
                return

    flags = [
        "--no-config",
        # "--color",
        "--verbose",
        f"--cache-dir={dld}",
        "--header",
        "--emit-trusted-host",
        "--emit-find-links",
        "--emit-index-url",
        "--emit-options",
        "--annotate",
        *((f"--constraint={aux}",) if fs.exists(aux) else ()),
        f"--extra={extras}",  # OR: --all-extras
        "--strip-extras",
        "--allow-unsafe",
        # --upgrade
        # --upgrade-package NAME
        # --generate-hashes --reuse-hashes
        # --rebuild --no-build-isolation
    ]
    import tempfile

    with tempfile.NamedTemporaryFile() as tmp:
        cmdv = [
            *exe,
            *flags,
            *(args or ()),
            f"--output-file={tmp.name}",
            "--",
            f"{src}",
        ]
        print("$ " + " ".join(cmdv))
        _ps = S.run(cmdv, check=True, text=True)
        # Windows: with os.open(tmp.name, os.O_RDONLY | os.O_BINARY | os.O_TEMPORARY) as bf:
        with open(tmp.name, "rb") as bf:
            out = bf.read()
        with open(tgt, "wb") as bf:
            bf.write(stamp)
            bf.write(out)


def sync_venv_to_requirements() -> None:
    # ERR:(ubuntu=20.04):  × Cannot uninstall apturl 0.5.2
    #   ╰─> It is a distutils installed project and thus we cannot accurately determine which files belong to it which would lead to only a partial uninstall.
    #   ::: FIXED: outside .venv use "pip-sync --user" or pip-sync --pip-args="--ignore-installed"
    #     [_] BUT: why I didn't need it earlier on ArchLinux ?
    # BET?(direct): from piptools.scripts import compile, sync; sync.cli(...)
    exe = ("pip-sync", "--user")
    ## FAIL:DISABLED: initial empty .venv don't have pip-tools installed
    # if sys.prefix != sys.base_prefix:
    #     exe = (sys.prefix + "/bin/python", "-m", "piptools", "sync", "--user")
    # else:
    #     # BAD: it's fine if pip-tools are installed into different python, only PATH matters
    #     exe = (sys.executable, "-m", "piptools", "sync", "--user")
    src = "dev-requirements.txt"
    # aux = "requirements.txt"
    tgt = fs.join(sys.prefix, "_stamp_pinned")

    import re

    with open(src, "r", encoding="utf-8") as f:
        text = f.read()
    lines = re.sub(r"\s*\\\n\s*", " ", text, flags=re.MULTILINE).split("\n")
    specs = (s for l in lines if (s := l.strip()) and not s.startswith("#"))
    pinned = "".join(s + "\n" for s in sorted(specs))

    if fs.exists(tgt):
        with open(tgt, "r", encoding="utf-8") as f:
            if pinned == f.read():
                return

    # --user --pip-args TEXT --python-executable TEXT -- ... f"{aux}"
    cmdv = [*exe, "--no-config", "--verbose", "--", f"{src}"]
    # BAD:(slow): from shutil import which; print(which(exe))
    print("$ " + " ".join(cmdv))
    _ps = S.run(cmdv, check=True, text=True)
    with open(tgt, "w", encoding="utf-8") as f:
        f.write(pinned)


def update_requirements(args: list[str] | None = None) -> None:
    # FIXME:PERF: yield early : don't even run hash-checks for chained RQs
    pin_requirements(
        tgt="dev-requirements.txt",
        aux="dev-requirements.txt",
        extras="all,dev",
        args=["--all-build-deps", *args],
    )
    pin_requirements(
        tgt="requirements.txt",
        aux="dev-requirements.txt",
        extras="default",
        args=args,
    )
    sync_venv_to_requirements()
