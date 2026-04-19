#
# SPDX-FileCopyrightText: 2016 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#

__all__ = [
    'BaseCommand',
    'WrongCommand',
]


class BaseCommand:
    cmd = 'command.base'


class WrongCommand:
    cmd = 'command.wrong'
