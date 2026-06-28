import os.path as fs

from .systems.logsystem import log

PKGSRC_DIR = fs.dirname(fs.realpath(__file__)) + "/"

__all__ = ["log"]
