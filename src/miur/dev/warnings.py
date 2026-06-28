import gc
import sys


def enable_warnings() -> None:
    gc.set_debug(gc.DEBUG_UNCOLLECTABLE)

    if not sys.warnoptions:
        import warnings

        # WHY: print a traceback pinpointing exactly where the unclosed file object was created.
        # warnings.simplefilter("always", ResourceWarning)

        # DEBUG: ResourceWarning(asyncio), DeprecationWarning(ipython), etc.
        warnings.simplefilter("always")  # OR="default" to print 1st only
        warnings.filterwarnings("error")  # Treat warnings as errors
        # warnings.resetwarnings()  # Back to default behavior
