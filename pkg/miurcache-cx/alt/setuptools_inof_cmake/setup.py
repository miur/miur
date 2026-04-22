## NEED:
# [build-system]
# requires = ["setuptools>=68", "wheel"]
# build-backend = "setuptools.build_meta"

from setuptools import Extension, setup

ext = Extension(
    name="graphcore",
    sources=["py_graphcore.c", "graphcore.c"],
    extra_compile_args=["-std=c2y", "-O3"],
    # define_macros=[("Py_LIMITED_API", "0x03090000")],
    # py_limited_api=True,
)

setup(
    ext_modules=[ext],
)

## WARN: setuptools dependency tracking for .h is not reliable → safest invariant: clean build when headers change
# uv pip install -e . && python -c "import importlib, graphcore; importlib.reload(graphcore)"

## ALT:BAD? no wheel, no dependency metadata, not portable
# cc -std=c2x -O3 -fPIC -shared \
#     $(python3 -c "import sysconfig; print(sysconfig.get_config_var('CFLAGS'))") \
#     py_graphcore.c graphcore.c \
#     -o graphcore$(python3-config --extension-suffix)
