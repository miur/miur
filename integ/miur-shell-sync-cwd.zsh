# vim:ft=zsh
# USAGE:CFG:(~/.zshrc):ADD: snippet to make integration/aliases work for !miur in !shell
#   _m=${${:-miur}:c:A:h:h}
#   [[ $_m == /usr/* ]] && _m="$_m/share/miur"
#   _m=$_m/integ/miur-shell-sync-cwd.zsh
#   if [[ -f $_m ]]; then builtin source $_m; fi; unset _m

if [[ -n ${MIUR_LEVEL-} ]]; then
  # NOTE: sync PWD when returning from shell_out (nested shell) back to !miur
  #   BET?(generalize):FUT: read /proc/$PID/cwd of exited child process
  zshexit(){ local f=${XDG_RUNTIME_DIR:?}/miur/cwd
    [[ -d ${f%/*} ]] || mkdir -m700 -p "${f%/*}"
    (set +C && print -n -- "$PWD" > "$f")  # Disable clobbering
  }

  # NOTE: source all aliases on first run
  if [[ $(alias m) != "m=miur" ]]; then
    source "${${(%):-%x}:a:h}/miur-shell-aliases.sh" =miur "silent"
  fi

  miur(){
    exit  # NOTE: exit miur-nested shell due to ${MIUR_LEVEL-}
  }

  # HACK: #miur※⡢⣯⢁⢏ pre-fill prompt (inof running) by specified cmdline on ZSH startup
  #   BET: print help notice  above shell-out to use $F / $S vars in cmdline
  if [[ -n ${MIUR_SHELLOUT_CMDLINE-} ]]; then
    print -z "$MIUR_SHELLOUT_CMDLINE"
    unset MIUR_SHELLOUT_CMDLINE
  fi

else # if [[ -n ${MIUR_LEVEL-} ]]

  # TODO:OPT:(flag): store both file:// "cwd" and miur:// "cwdurl"
  miur(){ local d f=${XDG_RUNTIME_DIR:?}/miur/cwd integ=${${(%):-%x}:a:h}
    [[ -d ${f%/*} ]] || mkdir -m700 -p "${f%/*}"
    # NOTE: source all aliases on first run in shell session
    [[ $(alias m) == "m=miur" ]] || source "$integ/miur-shell-aliases.sh" =miur "verbose"
    [[ -n ${MIUR_LEVEL-} ]] && exit  # NOTE: exit miur-nested shell
    # NOTE: load !miur back at whatever folder it was in previous session
    [[ -s $f ]] && d=$(<"$f") && [[ $d != $PWD ]] && [[ -e $d ]] && set -- "$d" "$@" && unset d
    command miur --choosedir="$f" "$@"
    # NOTE: change shell $PWD to the last viewed directory
    [[ -s $f ]] && d=$(<"$f") && [[ $d =~ / ]] && while [[ ! -d $d ]]; do d=${d%/*}; done
    if [[ -d ${d-} && $d != $PWD ]]; then builtin cd -- "$d" || return; fi
  }

fi # if [[ -n ${MIUR_LEVEL-} ]]
