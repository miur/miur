import os


def shell_out(loci: str, **kw: str) -> None:
    from .any_exe import run_tty_async, to_env

    return run_tty_async([os.environ.get("SHELL", "sh")], env=to_env(f=loci, **kw))


def shell_out_prompt(loci: str) -> None:
    # HACK※⡢⣯⢁⢏ pre-fill prompt (inof running) by specified cmdline on ZSH startup
    # RQ:(~/.zshrc): if [[ -n ${ZSH_BUFFER-} ]]; then print -z "$ZSH_BUFFER" && unset ZSH_BUFFER; fi
    #   [_] BET: print help notice  above shell-out to use $F,$D,$X,$S vars in cmdline
    #   ALSO: use $f2 to refer to 2nd tab, and $F to refer to previously opened tab
    shell_out(loci, ZSH_BUFFER=loci)
