import os
import os.path as fs
import subprocess as S

# ALT:(docker>=19.03): rootless mode (BAD: uses slow and hard-to-debug network topology)
#   [rootless] $ export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock
# ALT: $ id | grep -qwF docker || { sudo gpasswd -a "${USER:?}" docker; newgrp docker; }
#   FIXME: should add grp only if not there (nor real, nor eff); and run "newgrp" if there but not eff.
# ((UID)) || id "$user" | grep -qwF docker || docker(){ command $sudo docker "$@"; }
# ALT: $echo "$USER ALL=(root) NOPASSWD: /usr/bin/docker" | sudo tee -a /etc/sudoers.d/mydocker
#   REF: https://unix.stackexchange.com/questions/18830/how-to-run-a-specific-program-as-root-without-a-password-prompt

docker_exev = ["docker"]


def docker_build(tag: str, dofile: str, force: str) -> None:
    import datetime

    now = datetime.datetime.now().astimezone().replace(microsecond=0).isoformat()

    # ALT:(sha): git -C "$here" rev-parse --verify --default HEAD
    cmdv = (
        "git describe --always --long --first-parent --dirty --abbrev=10 --tags".split()
    )  # --match '*.*'
    vcstag = (
        S.run(cmdv, check=False, text=True, stdout=S.PIPE).stdout.strip()
        or "<nogit-standalone>"
    )
    ver = ".".join(vcstag.split("-")[:2])

    cmdv = [
        *docker_exev,
        "buildx",
        "build",
        *(  # >[!rebuild] invalidate e.g. stage="devapp" (and keep prev=baseuser,pkgdeps)
            ()
            if not force
            else ("--no-cache",) if force == "all" else (f"--no-cache-filter={force}",)
        ),
        "--tag=" + tag,
        "--network=host",
        "--force-rm=true",
        # "--progress=plain",
        f"--label=org.opencontainers.image.created={now}",
        f"--label=org.opencontainers.image.version={ver}",
        f"--label=org.opencontainers.image.revision={vcstag}",
        # "--build-context=pjroot=../",
        # "--build-context airy=https://github.com/amerlyq/airy.git"
        # --add-host="$myhost:127.0.0.1"
        ### DISABLED:(not needed): docker by DFL uses "local image store" (cmp to CI/CD)
        ## ALT:NEED:(running registry): "--cache-from=type=registry,ref=local/miur:buildcache"
        # WARN! ¦ size of the local cache will continue to grow
        #   REF: https://docs.docker.com/build/cache/backends/local/
        ## FAIL: default driver only supports the local cache if containerd is enabled
        # dcache = fs.join(os.environ["HOME"], ".local/share/docker/_cache")
        # "--cache-from=type=local,src=" + dcache,  # <[+caching]
        # "--cache-to=type=local,mode=max,compression=zstd,dest=" + dcache,
        ### NEED: _ps = S.run(..., input=txt)
        # "-",  # <[no-ctx] ALT: $ docker build -f- URL <<EOF
        "--file=" + dofile,
        ".",  # OR: pj-top-dir
    ]

    _ps = S.run(cmdv, check=True, text=True)


# TODO: reuse "src.integ.any_exe.to_cmdv()"
def docker_run(tag: str, argv: list[str]) -> None:
    import re

    entry = None
    if argv and argv[0] == "-":
        argv.pop(0)
        ## DISABLED: doesn't exist for "busybox" img
        # entry = "/usr/bin/bash"
        entry = "/bin/sh"

    wkdir = "/mnt"
    nm = re.sub(r"[^a-zA-Z0-9]", "_", tag[0]) + re.sub(r"[^a-zA-Z0-9_.-]", "_", tag[1:])
    # SEE:REF: docker container run | Docker Docs ⌇⡧⢺⡛⣺
    #   https://docs.docker.com/reference/cli/docker/container/run/#label
    cmdv = [
        *docker_exev,
        "run",
        "--name=" + nm,  # OR=miurapp
        "--rm",
        "--tty=true",
        "--interactive=true",
        "--read-only=true",  # <EXPL: prohibit any random writes (use host-binds and VOLUMEs)
        "--tmpfs=/tmp",  # TEMP:WKRND: allow writing /tmp/miur.pid file
        "--network=host",
        # --hostname="$myhost"
        # --add-host="$myhost:127.0.0.1"
        # --label com.example.foo=bar  # <REF: container's label
        # TRY: idmapping※⡧⢼⣍⡬ + driver-local※⡧⢼⣤⢏
        # FAIL:(,ro=true):ERR: must not set ReadOnly mode when using anonymous volumes
        #   FIXED? add volumen name "src=mymnt"
        # f"--mount=type=volume,dst={wkdir},volume-driver=local,volume-opt=device=.,volume-opt=type=bind,volume-opt=map-mount=b:1000:1010:1",
        # f"--mount=type=volume,dst={wkdir},volume-driver=local,volume-opt=type=bind,volume-opt=device=/mnt,volume-opt=map-users=0:1000:1,volume-opt=map-groups=0:1000:1",
        # f"--mount=type=volume,src=mymnt,dst={wkdir},ro=true,volume-driver=local,volume-opt=type=bind,volume-opt=device=/mnt,volume-opt=map-users=0:1000:1,volume-opt=map-groups=0:1000:1",
        # TRY:AGAIN: idmap with quoted "o"
        #   OK: $ sudo docker volume create --driver local --opt type=bind --opt device=/mnt --opt o=ro,X-mount.idmap=b:0:1000:1
        # FAIL: f'--mount=type=volume,src=mymnt,dst={wkdir},volume-driver=local,volume-opt=type=bind,volume-opt=device=/mnt,"volume-opt=o=ro,X-mount.idmap=b:0:1000:1"',
        #   ERR: failed to mount local volume: mount /mnt:/var/lib/docker/volumes/mymnt/_data: no such device.
        #     HYPO~ issue is due to "type=bind" i.e. real FS behind "_data" doesn't exist ?
        #       $ sudo cat /var/lib/docker/volumes/mymnt/opts.json
        #       {"MountType":"bind","MountOpts":"","MountDevice":"/mnt","Quota":{"Size":0}}%
        # f"--mount=type=volume,src=mymnt,dst={wkdir},volume-driver=local,volume-opt=type=bind,volume-opt=device=/mnt",
        #   $ sudo mount --bind --map-users 0:1000:1 --map-groups 0:1000:1 /root /mnt
        #   BAD: we still need unique UID/GID inside container, not present on host
        #     orse we won't be able to mount on host
        #     BUT: primary usecase is to map "myuser:1002" to existing "ubuntu:1000",
        #       to be able to forward it inside container as "dockeruser" -- which should be fine
        ## NOTE:(shared): allow bind-mounting over ./_build folder inside/outside of docker
        ##  OK: $ sudo mount --bind --map-users 0:1000:1 --map-groups 0:1000:1 /root /mnt
        #   f"--mount=type=bind,src=/mnt,dst={wkdir},ro=true,bind-propagation=rshared",  # ,bind-recursive=readonly
        f"--mount=type=bind,src=.,dst={wkdir},ro=true,bind-propagation=rshared",
        f"--mount=type=volume,src=miurhome,dst=/home/user,ro=false",  # ,volume-nocopy
        f"--env=TERM={os.environ['TERM']}",
        # f"--user={uid}:{gid}",
        f"--workdir={wkdir}",
    ]
    if entry:
        cmdv.append(f"--entrypoint={entry}")
    cmdv += ["--", tag, *argv]
    print("$ " + " ".join(cmdv))
    # OR: _ps = S.run(cmdv, check=True, text=True)
    os.execvp(cmdv[0], cmdv)


def docker_exists(tag: str) -> bool:
    cmdv = [*docker_exev, "image", "inspect", "--format={{.Id}}", "--", tag]
    ps = S.run(cmdv, check=False, text=True, stdout=S.DEVNULL, stderr=S.DEVNULL)
    return ps.returncode == 0


class was_changed:
    def __init__(self, stamp: str, dofile: str, force: str) -> None:
        self._stamp = stamp
        self._dofile = dofile
        self._force = force
        self._digest: str
        self._enc = "utf-8"

    def __enter__(self) -> str:
        with open(self._dofile, "r", encoding=self._enc) as f:
            txt = f.read()
        dat = "".join(
            sorted(
                k + os.linesep
                for l in txt.splitlines()
                if (k := l.strip()) and not k.startswith("#")
            )
        ).encode(self._enc)

        import hashlib

        h = hashlib.sha256(dat).hexdigest()
        self._digest = f"{self._dofile}:{len(dat)}:{h}"

        if self._force or not fs.exists(self._stamp):
            return txt
        with open(self._stamp, "r", encoding=self._enc) as f:
            digest = f.read()
        if digest != self._digest:
            return txt
        return ""

    def __exit__(self, _et, exc, _tb):  # type:ignore[no-untyped-def]
        if not exc and self._digest:
            with open(self._stamp, "w", encoding=self._enc) as f:
                f.write(self._digest)


def docker(pjdir: str, argv: list[str]) -> None:
    sock = os.environ.get("DOCKER_HOST", "unix:///var/run/docker.sock")
    assert sock.startswith("unix://")
    sock = sock.removeprefix("unix://")
    if not os.access(sock, os.R_OK | os.W_OK):
        # MAYBE:TRY?(sudo): !gosu ex~: https://docs.docker.com/build/building/best-practices/#add-or-copy
        docker_exev.insert(0, "sudo")

    dofile = fs.join(pjdir, "pkg", "docker", "Dockerfile")
    stamp = fs.join(pjdir, ".git", "_docker_stamp")
    tag = "local/miur:latest"

    # WKRND: rebuild and install each time when SRC changes
    force = ""
    if argv and argv[0] == ".":
        argv.pop(0)
        force = "devapp"

    # ALT: if not docker_exists(tag):
    # TEMP:DISABLED:FIXME: use deps from frozen.txt OR pypj.toml
    # BAD:FIXME? should also re-install project each time any SRC changes
    #   ALT:BET? during DEV-WF use special docker with "-e" installation
    with was_changed(stamp, dofile, force) as txt:
        if txt:
            docker_build(tag, dofile, force)
    docker_run(tag, argv)
