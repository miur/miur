import uuid
from subprocess import check_output, CalledProcessError
import os
import psutil

process = psutil.Process(os.getpid())


def meminfo():
    return process.memory_info().rss


def execute(cmd):
    try:
        out = check_output(cmd, shell=isinstance(cmd, str))
    except CalledProcessError:
        return None
    else:
        return out[:-1].decode('utf-8').splitlines()


# NEED:
#   insert items into satellite
#   use satellite info
def list2dom(items, parent=None):
    results = {uuid.uuid4(): e for e in items}
    edges = set(results.keys())
    if parent is not None:
        edges.add(parent)
    return (edges, results)


def cmd2dom(cmd, parent=None):
    return list2dom(execute(cmd), parent)


# NOTE: in-place replacing of prop by value on access
class cached_property(object):
    def __init__(self, method, name=None):
        self._method = method
        self._name = name or method.__name__

    def __get__(self, obj, cls=None):
        if obj is None:
            return self
        value = self._method(obj)
        setattr(obj, self._name, value)
        return value
