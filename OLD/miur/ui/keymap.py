#
# SPDX-FileCopyrightText: 2016 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#

# NOTE: controls may be individual for each window in view!
#   => bind to actual functions, without re-interpreting

# NOTE: each *UI* has its own keymap
#   => but you can combine/inherit between them
#       -- if you use more then one at once

keymap = {
    'j': 'focus-node-next',
    'k': 'focus-node-prev',
    'g': 'focus-node-beg',
    'G': 'focus-node-end',
    'h': 'shift-node-parent',
    'l': 'shift-node-current',
    'q': 'quit',
    '^J': 'quit',
}
