#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#

import argparse
import fcntl
import os
import sys

from .. import __appname__, __doc__
from ..ctl import create_instance


def options(args=None):
    parser = argparse.ArgumentParser(prog=__appname__, description=__doc__)
    opt = parser.add_argument
    opt('--pidfile', help="File to write PID of started process")
    return parser.parse_args(args=args)


def pipeguard(fn):
    try:
        fn()
    except BrokenPipeError:
        # Python flushes standard streams on exit; redirect remaining output
        # to devnull to avoid another BrokenPipeError at shutdown
        devnull = os.open(os.devnull, os.O_WRONLY)
        os.dup2(devnull, sys.stdout.fileno())
        sys.exit(1)  # Python exits with error code 1 on EPIPE


def main():
    opts = options(sys.argv[1:])
    # FAIL: must first lock file, and only then clean-up its content
    #   => otherwise running second instance of *miur* in same session will corrupt PIDfile
    with open(opts.pidfile, 'w') as pidf:
        # NOTE: keep file opened and locked until program dies -- to track session
        fcntl.flock(pidf, fcntl.LOCK_EX | fcntl.LOCK_NB)
        pidf.write(str(os.getpid()))
        pidf.flush()
        pipeguard(lambda: create_instance(opts))
