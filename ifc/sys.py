#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
__all__ = [
    'set_current_thread_name',
]

import inspect
import threading

def set_current_thread_name(nm=None):
    if nm is None:
        nm = inspect.currentframe().f_back.f_code.co_name

    threading.current_thread().name = nm

    # HACK:BAD:(docs): overwrites "sys.argv" to change process name
    # BET: use explicit shebang "#!/usr/bin/python" and substitute path in post-install hook (for virtualenv)
    #   OR by maintainer when building package for concrete distribution
    try:
        import prctl
        prctl.set_name(nm)  # __appname__ + "/" + nm
    except:
        pass
