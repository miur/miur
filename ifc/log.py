#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
__all__ = [
    "getLogger",
]

import logging
import sys


def getLogger(cmpt=None):
    if cmpt is None:
        # CHECK: in compiled Python code "inspect.currentframe()" don't work
        # ALT:TRY: log the calling function name by overriding logger.findCaller in a custom logger class.
        caller = sys._getframe(1).f_back
        func = caller.f_code.co_name
        mod = caller.f_globals["__name__"]
        cmpt = mod.partition(".")[2].partition(".")[0]
        cmpt = cmpt if cmpt else "root"

    ## OBSOLETE: logging.Formatter has %(process:d) %(thread:d)
    # pid = os.getpid()
    # tid = threading.get_native_id()

    # THINK: do we need individual logger instances per cmpt at all ?
    return logging.getLogger(cmpt)
