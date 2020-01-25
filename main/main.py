#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#

import argparse
import os
import sys

from .. import __appname__, __doc__
from ..ctl import create_instance


def options(args=None):
    parser = argparse.ArgumentParser(prog=__appname__, description=__doc__)
    opt = parser.add_argument
    opt('--pidfile', help="File to write PID of started process")
    return parser.parse_args(args=args)


def pipeguard(f):
    try:
        f()
    except BrokenPipeError:
        # Python flushes standard streams on exit; redirect remaining output
        # to devnull to avoid another BrokenPipeError at shutdown
        devnull = os.open(os.devnull, os.O_WRONLY)
        os.dup2(devnull, sys.stdout.fileno())
        sys.exit(1)  # Python exits with error code 1 on EPIPE


def main():
    opts = options(sys.argv[1:])
    with open(opts.pidfile, 'w') as pidf:
        # NOTE: keep file opened until program dies
        pidf.write(str(os.getpid()))
        pidf.flush()
        pipeguard(lambda: create_instance(opts))
