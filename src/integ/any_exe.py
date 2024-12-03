import os

# if globals().get("TYPE_CHECKING"):
from typing import Callable, Mapping, Sequence, Union

type ArgType = "Union[str, int, Sequence[str | int], Mapping[str, str | int]]"

## REF: repeated args for all popen-based functions
# python - Generate TypedDict from function's keyword arguments - Stack Overflow ⌇⡧⡄⡌⢶
#   https://stackoverflow.com/questions/63910610/generate-typeddict-from-functions-keyword-arguments
from typing import NotRequired, TypedDict, Unpack


class ExeKWArgs(TypedDict):
    # TRY:(cb): move into indy fn (like to_cmdv) inof passing-through ?
    input: NotRequired[str]
    env: NotRequired[Mapping[str, str]]
    cb: NotRequired[Callable[[], None]]
    # cwd
    # check
    # timeout
    # errors


def _insert_opts(cmdv: list[str], opts: Mapping[str, str | int]) -> list[str]:
    for k, v in opts.items():
        if len(k) == 1:
            cmdv.extend(("-" + k, str(v)))
        else:
            cmdv.append(f"--{k}={v}")
    return cmdv


def to_cmdv(*args: ArgType, **kw: str | int) -> list[str]:
    cmdv: list[str] = []
    for a in args:
        if isinstance(a, (str, int)):
            cmdv.append(str(a))
        elif isinstance(a, Sequence):
            cmdv.extend(x if isinstance(x, str) else str(x) for x in a)
        elif isinstance(a, Mapping):
            _insert_opts(cmdv, a)
        else:
            raise NotImplementedError(a)
    if kw:
        try:
            idx = cmdv.index("--")
        except IndexError:
            lopt = (i for i, c in reversed(list(enumerate(cmdv))) if c.startswith("-"))
            idx = next(lopt, 0)
        cmdv[idx:idx] = _insert_opts([], kw)
    return cmdv


# NOTE: for empty env use "to_env({}, arg=...)"
def to_env(
    # /,
    *args: "Mapping[str, str | int]",
    # RENAME?(base): inherit=, reset=
    base: "bool | Mapping[str, str] | None" = None,
    **kwargs: "str | int",
) -> dict[str, str] | None:
    # PERF: keep default args for subprocess.run(env=None)
    if not args and not kwargs and base is None:
        return None

    if base is None or base is True:
        base = dict(os.environ)
    elif base is False:  # minimal
        base = {
            "SHELL": "/bin/sh",
            "PATH": "/usr/bin",
            # LANG="en_US.UTF-8" USER="$USER" HOME="$HOME"
        }
    elif isinstance(base, Mapping):
        base = dict(base)
    else:
        raise TypeError(base)
    for a in [*args, kwargs]:
        for k, v in a.items():
            base[k] = v if isinstance(v, str) else str(v)
    return base


def run_bg_wait(
    cmdv: Sequence[str],
    /,
    *,
    split: bool | str | None = None,
    **kw: Unpack[ExeKWArgs],
) -> None | str | list[str]:
    from subprocess import DEVNULL, PIPE, run

    from ..app import g_app

    ps = run(
        cmdv,
        env=kw.get("env", None),
        check=True,
        text=True,
        input=kw.get("input", None),
        stdout=DEVNULL if split is None else PIPE,
        # OR: special "pipebuf"
        stderr=g_app.io.pipeerr or g_app.io.ttyalt,
    )
    if split is None:
        return None
    if split is False or split == "":
        return ps.stdout
    if split is True:
        return ps.stdout.splitlines()
    return ps.stdout.split(sep=split)


# def run_bg_async_callback():
#     coro = asyncio.create_subprocess_exec(*cmdv, env=envp,
#       stdin=DEVNULL or PIPE, stdout=DEVNULL or PIPE, stderr=ERRBUF)
#     # TODO:SPLIT: cb part vs loop-io
#     asyncio_primary_out(g_app, coro, cb)


def run_tty_async(cmdv: Sequence[str], /, **kw: Unpack[ExeKWArgs]) -> None:
    from ..app import g_app
    from ..curses_ext import shell_async
    from ..loop_asyncio import asyncio_primary_out

    coro = shell_async(
        g_app.stdscr,
        cmdv,
        interactive=True,
        input=kw.get("input", None),
        env=kw.get("env", None),
    )
    asyncio_primary_out(g_app, coro, cb=kw.get("cb", None))
