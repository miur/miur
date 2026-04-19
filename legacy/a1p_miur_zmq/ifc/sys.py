#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
__all__ = [
    'set_current_thread_name',
]

import inspect
import threading

from .. import __appname__


def set_current_thread_name(nm=None):
    if nm is None:
        nm = inspect.currentframe().f_back.f_code.co_name

    threading.current_thread().name = nm

    # FIXED: process name is overwritten by main thread name (e.g. by mainloop)
    #   https://nikhilism.com/post/2018/linux-main-thread-name/
    # BET:USE: composite name "process/thread" to distinguish from ZMQ, etc.
    # if threading.current_thread() == threading.main_thread():
    #     nm = __appname__

    try:
        from prctl import set_name
        set_name(__appname__ + '/' + nm)
    except:
        pass

    ## BET: use explicit shebang "#!/usr/bin/python" and substitute path in post-install hook (for virtualenv)
    #   OR by maintainer when building package for concrete distribution
    ## ALT: python-setproctitle BUT: the same as prctl.set_name(nm)
    ## BAD:HACK:(docs): overwrites "sys.argv" to change process name -- lost other argv
    ## FAIL: affects one more thread beside main one
    # try:
    #     from setproctitle import setproctitle
    #     setproctitle(__appname__)
    # except:
    #     pass
