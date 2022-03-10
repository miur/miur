# WORKFLOW(fragments)
import asyncio
import subprocess as P
from typing import Any

from just.iji.shell import runlines


def handle_keybindings(wg: Any, key: str | int) -> None:
    move_frag(wg, key)
    pacman_frag(wg, key)


def pacman_frag(wg: Any, key: str | int) -> None:
    # TRY: async wait for subprocess result
    # https://docs.python.org/3/library/asyncio-subprocess.html#asyncio.create_subprocess_exec
    def exe(cmdline: str) -> None:
        print("\n".join(runlines(cmdline.split() + [str(wg.item)])))

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

            # [_] FIXME:FIND: work with sudo in python
            # exe("sudo pacman -Rsu")
            # WKRND: total mess with input when redirecting fd0/fd1 into separate newterm
            _ret = P.run(
                "sudo pacman -Rsu".split() + [str(wg.item)],
                stdin=sys.stdin,
                stdout=sys.stdout,
                stderr=sys.stderr,
                check=True,
            )
            wg._scroll._provider.refresh()
            # event.set()
            print("[DONE]")

        asyncio.get_running_loop().run_in_executor(None, pkguninstall)


def move_frag(wg: Any, key: str | int) -> None:
    # ---
    if key == "j":
        wg.pos += 1
    if key == "k":
        wg.pos -= 1

    # [_] FUTURE: wg.pos = -1
    if key == "g":
        wg.pos = -len(wg)
    if key == "G":
        wg.pos = len(wg)
    if key == "H":
        wg.pos = 0
    if key == "M":
        wg.pos = wg._scroll.height // 2
    if key == "L":
        wg.pos = wg._scroll.height
