# WORKFLOW(fragments)
import asyncio
import subprocess as P
from typing import Any

# from just.use.iji.shell import runlines
from just.use.airy.secu import sudorun


def handle_keybindings(wg: Any, key: str | int) -> None:
    move_frag(wg, key)
    pacman_frag(wg, key)


def pacman_frag(wg: Any, key: str | int) -> None:
    # TRY: async wait for subprocess result
    # https://docs.python.org/3/library/asyncio-subprocess.html#asyncio.create_subprocess_exec
    def exe(cmdline: str) -> None:
        # NOTE: capture and combine interspersed stdout/stderr
        #   BAD: can't add prefix to all stderr lines
        cmd = cmdline.split() + [wg.item.name]
        print(P.run(cmd, check=True, stdout=P.PIPE, stderr=P.STDOUT, text=True).stdout)

    if key == "\n":
        exe("echo")
    if key == "i":  # pacq
        exe("pacman -Qi")
    if key == "u":  # pacl
        exe("pacman -Ql")

    if key == "f":  # pacR1+o
        exe("pactree --color --depth 1 --optional --reverse")
    if key == "d":  # pacr1+o
        exe("pactree --color --depth 1 --optional")
    if key == "s":  # pacr1
        exe("pactree --color --depth 1")
    if key == "a":  # pacr
        exe("pactree --color --depth 2")
    if key == "A":  # pacr
        exe("pactree --color")
    if key == "r":  # pacR+o
        exe("pactree --color --optional --reverse")

    if key == "x":  # pacx

        def pkguninstall() -> None:
            import sys

            # [_] FIXME:FIND: work with sudo in python/jupyter
            # exe("sudo pacman -Rsu")
            # WKRND: total mess with input when redirecting fd0/fd1 into separate newterm
            # BAD:ERR: Future exception was never retrieved
            # FIXED: why only one of stdout/stderr is redirected to !jupyter and another to !st ?
            # sudorun("sudo pacman -Rsu".split() + [wg.item.name], stdin=sys.stdin, stdout=sys.stdout, stderr=sys.stderr)
            P.run("sudo pacman -Rsu".split() + [wg.item.name], stdin=sys.stdin)
            wg._scroll._provider.refresh()
            # event.set()
            print("[DONE]")

        asyncio.get_running_loop().run_in_executor(None, pkguninstall)

    if key == "o":  # reverse sorting
        wg._scroll._provider.sortby(rev="!")
    if key == "O":  # switch sorting method
        wg._scroll._provider.sortby(arg="+")


def move_frag(wg: Any, key: str | int) -> None:
    # ---
    if key == "j":
        wg.currel += 1
    if key == "k":
        wg.currel -= 1
    if key == "h":
        wg.currel -= 10
    if key == "l":
        wg.currel += 10

    # [_] FUTURE: wg.pos = -1
    if key in ("g", 262):  # = <Home>
        # wg.pos = -len(wg)
        wg.curabs = 0
    if key == "G":
        # wg.pos = len(wg)
        wg.curabs = None
    if key == "H":
        wg.pos = 0
    if key == "M":
        wg.pos = wg._scroll.height // 2
    if key == "L":
        wg.pos = wg._scroll.height - 1
    if key in ("J", "\x06", 338):  # = <C-f>, <PgDn>
        # wg.pos += wg._scroll.height
        wg.curoff += 1
    if key in ("K", "\x02", 339):  # = <C-b>, <PgUp>
        # wg.pos -= wg._scroll.height
        wg.curoff -= 1
