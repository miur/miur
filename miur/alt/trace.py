import sys
import logging


# WARN: modifying
TRACE = 5
trace = (lambda *a: logging.getLogger('trace').log(TRACE, *a))
logging.addLevelName(TRACE, 'TRACE')


def toggle(enable=None):
    if enable is None:
        enable = sys.getprofile()
    tracefunc.depth = 1  # NEED: reset for deterministic depth in any moment
    sys.setprofile(tracefunc if enable else None)


def setloglevel(lvl=logging.INFO):
    toggle(lvl <= TRACE)
    return logging.getLogger().setLevel(lvl)


# TODO: toggle tracefunc and change loglevel by keybindings
# https://stackoverflow.com/questions/8315389/how-do-i-print-functions-as-they-are-called
# http://www.dalkescientific.com/writings/diary/archive/2005/04/20/tracing_python_code.html
# https://gist.github.com/brendano/16173
def tracefunc(frame, event, arg):
    if event not in ['call', 'return']:
        return
    if frame.f_code.co_filename.startswith('/usr/lib'):
        return
    if frame.f_code.co_name in ['<genexpr>', '<lambda>']:
        return

    if event == 'call':
        tracefunc.depth += 1
    module = frame.f_code.co_filename
    width = 30
    trace('{d:2d}{s} {name:{w}} | {file}:{lnum}'.format(
        d=tracefunc.depth,
        w=(width if tracefunc.depth <= 0 else width - tracefunc.depth),
        s=('>' if event == 'call' else '<') + (' ' * tracefunc.depth),
        name=frame.f_code.co_name,
        file=module[(module.rindex('/') + 1):],
        lnum=frame.f_code.co_firstlineno))
    if event != 'call':
        tracefunc.depth -= 1
