#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: CC0-1.0
#
"""
Basic facilities for each module
"""
__appname__ = "base"
__version__ = "0.0.4"

# HACK: hide module name 'sys', 'log', etc.
__all__ = [
    'getLogger',
    'set_current_thread_name',
]

from .log import *
from .sys import *
