#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: CC0-1.0
#
"""
Central communication hub
"""
__appname__ = "broker"
__version__ = "0.0.2"

# HACK: hide __appname__, etc for 'from broker import *'
__all__ = [
    'broker_hub',
]

from .hub import broker_hub
