#!/bin/sh
#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
#%SUMMARY: standalone sh app boilerplate
#%USAGE: $ ./$0
set -o errexit -o noclobber -o noglob -o nounset

exe=$(realpath -e "$0")

export PYTHONPATH="${exe%/*/*}/pkg${PYTHONPATH:+:$PYTHONPATH}"

exec python -m miur.main.main
