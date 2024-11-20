#!bash
#%USAGE: $ . =miur

if ! (return 0 2>/dev/null); then
  # set -o errexit -o errtrace -o noclobber -o noglob -o nounset -o pipefail
  echo "ERR: '$0' is not supposed to be run by $SHELL"
  exit -2
fi

app=$(realpath -e "${1:?}")
pj=/d/miur
if [[ $app != $1 ]]; then
  echo 'FUT: derive "pj" as a dif bw "$1" and realpath "$app"'
  exit 3
fi

this=$(realpath -e "${BASH_SOURCE[0]:-$0}")
root=${this%/*/*}
[[ $pj -ef $root ]] || pj=$root
this=$pj/${this#$root/}
app=$pj/${app#$root/}

echo "\033[33;1;4m[${this}]\033[m"

_ps4=$PS4 && PS4=' ' && set -x


alias miur.pkg="$pj/pkg/PKGBUILD.dev"
alias miur.impt="python -SIB -Ximporttime -- '$app'"
alias miur.prof="python -SIB -m cProfile -s cumulative -- '$app'"

## alias mi="$this"
## alias mi='miur'
## alias mi-='miur-'

alias m='miur'
alias m-='mi-'
alias ma='miur -a'
alias mb='miur --bare'
alias mj='miur -a -I'  # auto-connected to -K
alias mk='miur -a -K --logredir /t/miur.log'
alias ml='tail -F /t/miur.log'

if [[ ${ZSH_NAME:+zsh} ]]; then
    alias -g M='|miur'
fi


set +x && PS4=$_ps4
unset app _at this pj _ps4
return 0
