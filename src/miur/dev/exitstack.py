"""An ExitStack that permanently prevents traceback erasure.
Guarantees that failing callbacks, files, and subprocess contexts
preserve their exact deep internal source frames.

ex~: Why Python Erases the Traceback HereIn Python, when you call raise exc on an exception stored in a
variable outside of the original except block where it occurred, the Python interpreter invokes its default
reraising behavior.Because the execution context has shifted away from the _exit_wrapper frame, Python
truncates the bottom of the traceback, discarding the execution history of _exit_wrapper and wtty.close().
It replaces it with a single new frame pointing directly to the raise exc line in contextlib.py
"""

import functools
import traceback
from collections.abc import Iterable
from types import FrameType, TracebackType
from typing import Any

# 1. Capture the original lower-level frame stack extractor
_original_extract = traceback.StackSummary.extract


@functools.wraps(_original_extract)
def _unwrapping_extract(
    frame_gen: Iterable[tuple[FrameType, int] | TracebackType],
    /,
    *args: Any,
    **kwargs: Any,
) -> traceback.StackSummary:
    """Universally unwraps any execution frame containing a __wrapped__ target local variable."""

    # Materialize the initial standard sequence generator to avoid consuming generators twice
    frames = list(frame_gen)
    new_frames: list[traceback.FrameSummary] = []

    for item in frames:
        # Convert raw FrameType, TracebackType, or Tuple objects to a standard FrameType
        f: FrameType = (
            item.tb_frame
            if isinstance(item, TracebackType)
            else item[0]
            if isinstance(item, tuple)
            else item
        )

        # ROOT FIX: Use the _original_extract reference to calculate the standard
        # FrameSummary, safely avoiding the infinite recursion loop.
        standard_summary_list = _original_extract([(f, f.f_lineno)], *args, **kwargs)
        if standard_summary_list:
            new_frames.append(standard_summary_list[0])

        # Access the local scope of the active frame block
        locals_dict = f.f_locals

        # Locate contextlib's implicit `_exit_wrapper` or general wrapped callables
        callback_obj = locals_dict.get("callback")
        wrapped_target = getattr(callback_obj, "__wrapped__", None)

        if wrapped_target is not None:
            func_code = getattr(wrapped_target, "__code__", None)

            if func_code is not None:
                # Custom Python function: inject frame targeting the actual source definition line
                synthetic_frame = traceback.FrameSummary(
                    filename=func_code.co_filename,
                    lineno=func_code.co_firstlineno,
                    name=getattr(
                        wrapped_target, "__qualname__", wrapped_target.__name__
                    ),
                    lookup_line=True,
                )
                new_frames.append(synthetic_frame)
            else:
                # Built-in C method (like wtty.close): look up exactly where
                # this specific wrapper was called from by shifting back up the frame chain!
                parent_frame = f.f_back
                if parent_frame is not None:
                    synthetic_frame = traceback.FrameSummary(
                        filename=parent_frame.f_code.co_filename,
                        lineno=parent_frame.f_lineno,
                        name=f"{getattr(wrapped_target, '__qualname__', str(wrapped_target))} (via wrapper call-site)",
                        lookup_line=True,
                    )
                    new_frames.append(synthetic_frame)

    return traceback.StackSummary.from_list(new_frames)


def enable_erasure_guardians() -> None:
    traceback.StackSummary.extract = _unwrapping_extract  # type: ignore[assignment]
