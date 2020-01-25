#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
import os
import os.path as fs


# NOTE: "path" is uid in fs coords
class FSEntity(object):
    def __init__(self, path):
        self._path = path

    @property
    def path(self):
        return self._path

    @property
    def name(self):
        return fs.basename(self._path)


class FileEntity(FSEntity):
    pass


class DirEntity(FSEntity):
    def __iter__(self):
        # WARN: e.path is relative only when self._path relative
        with os.scandir(self._path) as it:
            for e in it:
                yield (DirEntity if e.is_dir() else FileEntity)(e.path)
