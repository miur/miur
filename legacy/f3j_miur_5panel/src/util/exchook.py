import sys
from contextlib import contextmanager

if globals().get("TYPE_CHECKING"):
    from types import TracebackType
    from typing import Any, Callable, Iterator, Optional, Type

# def raise_(exc: BaseException) -> None:
#     raise exc


@contextmanager
def enable_warnings(error: bool = True) -> "Iterator[None]":
    if sys.warnoptions:
        return

    import warnings

    # DEBUG: ResourceWarning(asyncio), DeprecationWarning(ipython), etc.
    if not error:
        warnings.simplefilter("always")  # OR="default" to print 1st only
        return

    # SRC: https://stackoverflow.com/questions/22373927/get-traceback-of-warnings
    # def warn_with_traceback(message, category, filename, lineno, file=None, line=None):
    #     log = file if hasattr(file,'write') else sys.stderr
    #     traceback.print_stack(file=log)
    #     log.write(warnings.formatwarning(message, category, filename, lineno, line))
    # warnings.showwarning = warn_with_traceback

    warnings.filterwarnings("error")  # Treat warnings as errors
    try:
        yield
    # except Warning:
    #     log.warning(traceback.format_exc())  # print traceback
    finally:
        warnings.resetwarnings()  # Back to default behavior


# FIXME: forward parent's callsite loci into log.* to know where exception was caught
def log_exc(value: "BaseException") -> "Any":
    # pylint:disable=import-outside-toplevel
    import os
    import traceback as TR
    from os import linesep as NL

    from .logger import TERMSTYLE, LogLevel, log

    try:
        termsz = os.get_terminal_size()
    except OSError:
        termsz = os.terminal_size((0, 0))
        log.warning("Can't reflow on narrow terminal")

    # TEMP: verify everything we have is actually printed with backtrace
    log.error("<Exception>")
    if hasattr(value, "__notes__"):
        log.error("+__notes__+")
    if value.__cause__:
        log.error("+__cause__+")

    msg = (str(value.args[0]) if value.args else "") + "".join(
        f"{NL}  \\ {k} = {v}" for k, v in vars(value).items()
    )
    value.args = (msg, *value.args[1:])

    # NOTE:("ignorables"): hide the top of the exception stack
    #   == everything above one of any regular happy-path key points
    # RENAME? ignored_srcframes | ignored_happy_path
    # MAYBE: store tuple (module_qualname, srcline) to
    ignorables = [
        "sys.exit(as_pkg_or_exe(select_entrypoint)())",
        "return lambda: miur_argparse(argv)",
        "return miur_frontend(g_app)",
        "return miur_main(g)",
    ]

    # MAYBE: write unconditionally to tty/stderr
    # _orig_write = log.write
    # log.config(write=g.io.ttyout.write)
    try:
        # ALT:DFL: log.error("".join(TR.format_exception(value, chain=True)))

        due: BaseException | None = value
        _cc = TERMSTYLE[LogLevel.COMMENT]
        _ct = TERMSTYLE[LogLevel.TRACE]
        _ce = TERMSTYLE[LogLevel.ERROR]
        _r = TERMSTYLE[None]
        tabl = 60
        while due:
            # tb = TR.extract_tb(due.__traceback__)

            ## FUT:ENH: print with tidbits for __context__, etc.  from OFF:SRC:
            ##   /usr/lib/python3.12/traceback.py:944:def format(self, *, chain=True, _ctx=None):
            te = TR.TracebackException.from_exception(due, capture_locals=True)
            tb = te.stack

            # bt = "".join(TR.format_tb(tb)).rstrip().replace(NL, NL + "\\")
            # log.info("Traceback (most recent call last):" + NL + "\\" + bt)
            last = len(tb) - 1
            lastignored: tuple[int, TR.FrameSummary] | None = None
            for i, t in enumerate(tb):
                # NOTE:("ignorables"): hide the top of the exception stack
                #   == everything above one of any regular happy-path key points
                if (
                    i < len(ign := ignorables)
                    and ign[i]
                    and (l := t.line)
                    and l.startswith(ign[i])
                ):
                    lastignored = (i, t)
                    continue
                if lastignored:
                    li, _lt = lastignored
                    log.info(f"... (collapsed {li+1}/{last+1} regular frames)")
                    lastignored = None

                modnm = t.filename.rpartition("/")[2]
                lnum = str(t.lineno)
                if t.lineno != t.end_lineno:
                    lnum += f"-{t.end_lineno}"
                lvl = LogLevel.TRACE
                _cs = _ct + "\033[3m"
                sep = "|"
                sfx = ""
                if i == last:
                    lvl = LogLevel.ERROR
                    _cs = _ce + "\033[1;3m"  # BAD(;4): makes underscores hard to see
                    # sep = ">"  # "\n"
                    if ctx := t.locals:
                        sfx = "".join(
                            f"\n\t{k} = {_ct}{v:.40s}{_r}"
                            for k, v in sorted(ctx.items())
                        )
                # NOTE: use unstripped srcline
                l = t._original_lines  # pylint:disable=protected-access
                l = "<NOT SRC>" if l is None else l.rstrip()
                indent = l[: len(l) - len(l.lstrip())]
                code = (
                    f"{indent}{_cc}{l[len(indent):t.colno]}"
                    + f"{_cs}{l[t.colno:t.end_colno]}{_r}"
                    + f"{_cc}{l[t.end_colno:]}{_r}"
                )
                if termsz.columns > 80:
                    fmt = f"{_r}{sep}{code}{" "*max(2,tabl-len(l))}{_cc}{f'// {t.name}'}{_r}{sfx}"
                else:
                    fmt = f"{_r}{_cc}{f'// {t.name}'}{_r}\n{sep}{code}{sfx}"
                log.at(
                    lvl,
                    fmt,
                    loci=f" {modnm}:{lnum}",
                )

            # err = "".join(TR.format_exception_only(type(due), due)).rstrip()
            err = "".join(te.format_exception_only()).rstrip()
            ## CHECK: it seems they are already appended ?
            if hasattr(due, "__notes__"):
                err += "".join(NL + "  \\ " + note for note in due.__notes__)
            log.error(err)

            due = due.__cause__ or due.__context__
            if due:
                log.warning(":Chained-From:")  # TEMP

        ## ALSO:MAYBE:
        # _orig_excepthook(etype, value, tb)  # OR: sys.__excepthook__(...)
    finally:
        # log.config(write=_orig_write)
        log.error("</Exception>")  # TEMP


def exception_handler(
    _etype: "Type[BaseException]",
    value: "BaseException",
    _tb: "Optional[TracebackType]",
) -> "Any":
    log_exc(value)


@contextmanager
def log_excepthook() -> (
    "Iterator[Callable[[Type[BaseException], BaseException, TracebackType], Any]]"
):
    _orig_excepthook = sys.excepthook
    try:
        sys.excepthook = exception_handler
        yield exception_handler
    finally:
        pass
        ## DISABLED: otherwise top-lvl exceptions won't be logged
        # sys.excepthook = _orig_excepthook
