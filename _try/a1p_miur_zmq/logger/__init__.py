#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
"""
Collect logs from all modules (also log all communication events)
"""
__appname__ = "logger"
__version__ = "0.0.5"

from .logger import logger_sink, logger_events
from .setup import setup_logging
