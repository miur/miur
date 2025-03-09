#!/usr/bin/env -S sudo docker build --tag=miur --force-rm=true .. --file
#%USAGE: $ docker run --name=miur --rm -ti --network=host -v ".:/mnt:ro" -- miur

######################################################################
FROM python:3.13-slim AS baseuser

# NOTE: use dir!=/mnt to prevent hiding all accidentally created files by later bind-mount
# ALT: /build /miur /mnt /boot /root /srv /opt /var/opt /var/tmp
WORKDIR /mnt
SHELL ["/usr/bin/bash", "-euo", "pipefail", "-c"]

ENV LANG="en_US.UTF-8" \
    LC_ALL="C"

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
<<"EOF"
  export DEBIAN_FRONTEND=noninteractive
  echo "debconf debconf/frontend select Noninteractive" | debconf-set-selections
  printf '%s\n' 'APT::Install-Recommends "0";' 'APT::Install-Suggests "0";' >> /etc/apt/apt.conf
  apt-get update && apt-get -yq install locales tzdata ca-certificates
EOF


RUN --network=none <<"EOF"
  ln -svfT bash /usr/bin/sh
  echo "${LANG:?} UTF-8" >> /etc/locale.gen
  export DEBIAN_FRONTEND=noninteractive
  dpkg-reconfigure locales
  update-locale "LANG=${LANG}"
  ln -svfT /usr/share/zoneinfo/UTC /etc/localtime
  dpkg-reconfigure tzdata
  update-ca-certificates
EOF


ARG myuser=user
ARG myuid=1000
ARG mygid=1000

RUN --network=none <<"EOF"
  groupadd --gid "${mygid:?}" "${myuser:?}"
  useradd --create-home --home-dir /home/user --shell /usr/bin/bash --no-log-init \
    --uid "${myuid:?}" --gid "$myuser" --groups "users" --comment "user@miur" -- "$myuser"
  chown "$myuser:$myuser" -- /opt
  ln -sT -- /opt /d
EOF

######################################################################
FROM baseuser AS pkgdeps

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
<<"EOF"
  pkgs=(
    git
    less
    ncurses-base  # for terminfo ($TERM)
    neovim
    zsh
    # build-essential
    # "cmake(>=3.24)"
  )
  if [[ ${pkgs[@]:+x} ]]; then apt-get update && apt-get -yq satisfy "${pkgs[@]}"; fi
EOF


######################################################################
FROM pkgdeps AS airycfg

USER "${myuser:?}"

ENV SHELL="/usr/bin/zsh" \
    EDITOR="nvim" \
    PAGER="less" \
    LESS="-A -i -j.5 -M -R -S -W -z-2 -x8"
    # OR: LESS="-a -i -j6 -M -N -R -S -w -z-4 -x4"

# TODO: copy gh:airy-cfg for zsh/nvim into /home/user
## OR:USE: "--build-context airy=https://github.com/amerlyq/airy.git"
## OR:BET? "git clone" in RUN cmd with --tmpfs or --cache
# ADD git@github.com:amerlyq/airy.git#main:zsh /home/user/my/zsh
ADD https://github.com/amerlyq/airy.git /opt/airy

RUN --network=none <<"EOF"
  ln -sT /opt/airy/zsh/cfg/zshrc ~/.zshrc
  ln -sT /opt/airy/zsh/cfg/zlogin ~/.zlogin
  ln -sT /opt/airy/zsh/cfg/aliases ~/.zsh_aliases
  ## WARN:PERF: store downloaded .tar in --cache (to avoid re-dld -- and to not store it in docker .img)
  # curl -LOJ "$(curl -s "https://api.github.com/repos/neovim/neovim/releases/latest" | awk -F '"' '/browser_download_url.*linux64.*gz"/{print$(NF-1)}')"
  # tar xzvf nvim-linux64.tar.gz
  # ln -sT ~/me/inst/nvim-linux64/bin/nvim ~/me/bin/nvim
  # ln -sT nvim ~/me/bin/v
  # sudo apt install cargo
  # git clone https://github.com/amerlyq/plugins
  # (cd plugins && ./update && ./recache)
  # git clone https://github.com/amerlyq/nou.vim
  # ln -s /d/airy/{nvim,ranger} -t ~/.config
  # sudo apt install universal-ctags tmux
  # mkdir ~/.config/tmux
  # ln -svft ~/.config/tmux -- ~/me/aura/airy/tmux/tmux.conf
  # curl -Lo ripgrep.deb "$(curl -s "https://api.github.com/repos/BurntSushi/ripgrep/releases/latest" | awk -F '"' '/browser_download_url.*amd64.deb"/{print$(NF-1)}')"
  # sudo apt install -y ./ripgrep.deb
EOF

######################################################################
FROM airycfg AS pipvenv

# ALT: use myvenv=~/.venv OR =~/miurvenv (to avoid pre-chown and to share)
#   BAD: user may want to purge configs in $HOME but keep venv and pipcache
# ALT:BET? ARG myvenv=/usr/local
ARG myvenv=/opt/venv

ENV VIRTUAL_ENV="${myvenv:?}" \
  PATH="${myvenv:?}/bin:/usr/local/bin:/usr/bin"

RUN --network=none \
  python3 -m venv "${myvenv:?}"

# DFL: PIP_CACHE_DIR=$HOME/.pip/cache OR:BET? set whole XDG_CACHE_HOME
ARG PIP_CACHE_DIR="/opt/pipcache"
ARG PIP_REQUIRE_VIRTUALENV="true"

# BET?PERF:TRY: put whole /opt/venv into VOLUME and only pip-sync it here
#   BAD: probably impossible during build-time
RUN --mount=type=bind,source=pkg/pypi/requirements.txt,target=requirements.txt \
    --mount=type=cache,target=${PIP_CACHE_DIR:?},uid=${myuid:?},gid=${mygid:?} \
  echo "${PIP_REQUIRE_VIRTUALENV:?}" && \
  pip install --upgrade pip hatchling \
  && pip check --disable-pip-version-check \
  && pip install --no-deps --requirement requirements.txt \
  && pip cache list \
  ;


######################################################################
# NOT:(busybox):COS: !miur goal is to aug DEV:WF/tools on regular distro
FROM pipvenv AS devapp

LABEL org.opencontainers.image.title="miur + venv (self-contained)"
LABEL org.opencontainers.image.url="https://github.com/miur/miur"
LABEL org.opencontainers.image.documentation="https://github.com/miur/miur"
LABEL org.opencontainers.image.source="https://github.com/miur/miur"
LABEL org.opencontainers.image.authors="amerlyq plus miurdocker at gmail dot com"
LABEL org.opencontainers.image.licenses="GPL-3.0-only"


## ERR: UndefinedVar: Usage of undefined variable '$XDG_STATE_HOME' (line 130)
# ARG MIUR_STATE_HOME="${XDG_STATE_HOME:-/home/user/.local/state}/miur"
ARG MIUR_STATE_HOME="/home/user/.local/state/miur"
## DFL: install miur w/o demo deps (OPT: change ARG to install everything)
# ARG mycmpts=all,dev
ARG mycmpts=default

RUN --network=none \
    --mount=type=bind,target=. \
    --mount=type=cache,target=${PIP_CACHE_DIR:?},uid=${myuid:?},gid=${mygid:?} \
  mkdir -p "${MIUR_STATE_HOME:?}" \
  && pip install --no-deps --no-build-isolation ".${mycmpts:+[$mycmpts]}"

## NOTE: use whole $HOME as persistent cache COS: !ranger and !nvim need RW there
# ALT? store state in "/miur/*" OR "${XDG_RUNTIME_DIR:-/run/user/$UID}/miur/*"
VOLUME ["/home/user"]

# TEMP:FIXME: avoid this var in ~/.zshrc
ENV XDG_RUNTIME_DIR="/tmp"

WORKDIR /mnt
ENTRYPOINT ["/usr/bin/zsh", "-lc", "python -m miur"]
CMD \
  [ "--remember-hist=${MIUR_STATE_HOME:?}/hist" \
  , "--remember-url=${MIUR_STATE_HOME:?}/url" \
  , "--choosedir=${MIUR_STATE_HOME:?}/cwd" \
  ]
