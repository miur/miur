#!zsh
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

  miur(){
    ## NOTE: source all aliases on first run in shell session
    #   WARN: do it before everything else to avoid overriding local vars
    #   WARN: sourcing inside function may not work in some shells
    local integ=${${(%):-%x}:a:h}
    [[ $(alias m) == "m=miur" ]] || source "$integ/miur-shell-aliases.sh" =miur "verbose"

    local d=${XDG_RUNTIME_DIR:?}/miur
    local f=${d:?}/cwd
    [[ -d $d ]] || mkdir -m700 -p "$d"
    [[ -n ${MIUR_LEVEL-} ]] && exit  # NOTE: exit miur-nested shell
    # NOTE: load !miur back at whatever folder it was in previous session
    [[ -s ${f:?} ]] && local c=$(<"$f") && [[ $c != $PWD ]] && [[ -e $c ]] && set -- "$c" "$@" && unset c
    # NOTE:OPT:(--remember-url): store both file:// "cwd" and miur:// "url"
    command miur \
      --remember-hist="$d/hist" \
      --remember-url="$d/url" \
      --choosedir="$f" \
      "$@"
    # NOTE: change shell $PWD to the last viewed directory
    [[ -s ${f:?} ]] && local c=$(<"$f") && while [[ $c != ${c%/*} ]]; do [[ -d $c ]] && break || c=${c%/*}; done
    if [[ -d ${c-} && $c != $PWD ]]; then builtin cd -- "$c" || return; fi
  }

fi # if [[ -n ${MIUR_LEVEL-} ]]
