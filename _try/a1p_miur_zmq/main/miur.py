#!/usr/bin/env python
#
# SPDX-FileCopyrightText: 2020 Dmytro Kolomoiets <amerlyq+code@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-only
#
"""
Standalone python app boilerplate
"""

# FROM: https://stackoverflow.com/questions/16981921/relative-imports-in-python-3
#   $ export PYTHONPATH=/path/before/miur $ OR: $ python -m miur.miur
#   SRC: https://gist.github.com/vaultah/d63cb4c86be2774377aa674b009f759a
if __name__ == '__main__' and __package__ is None:
    import os.path as fs, sys
    here = fs.abspath(fs.dirname(__file__))
    top = fs.join(fs.dirname(here), 'pkg')
    sys.path.insert(0, top)
    sys.path.remove(here)
    __package__ = 'miur.' + fs.basename(here)

from . import main

if __name__ == "__main__":
    main.main()
