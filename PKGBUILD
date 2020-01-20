# vim:ft=sh
#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: MIT
#
# Maintainer: Dmytro Kolomoiets <amerlyq@gmail.com>
#
# shellcheck shell=bash disable=SC2034,SC2154
pkgbase=miur
pkgname=miur
pkgver=0.0.1
pkgrel=1
pkgdesc='Modular interface for universal tree manipulation (file manager)'
url='https://github.com/miur/miur/'
license=('GPL-3.0-only' 'CC0-1.0' 'CC-BY-SA-4.0')
arch=('any')

depends=(
  # python-blessed
  python-prctl
  python-pyzmq
  python-urwid
)

optdepends=(
)

package() {
  prefix=$pkgdir/usr
  datadir=$prefix/share
  # make install prefix="$prefix"
  install -Dm644 -t "$datadir/licenses/$pkgbase" -- $(printf 'LICENSES/%s.txt\n' "${license[@]}")
  install -Dm644 -t "$datadir/doc/$pkgbase" -- README.rst
}
