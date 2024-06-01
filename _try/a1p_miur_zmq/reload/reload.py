#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
# TEMP: collection of hacks to reload python runtime
#

import logging

def reload():
    """reset logger when reloading code in runtime"""
    root = logging.getLogger()
    for h in root.handlers[:]:
        root.removeHandler(h)
    for f in root.filters[:]:
        root.removeFilter(f)
