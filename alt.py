#!/usr/bin/python3

import sys
import logging

import miur.alt as M


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

    global _depth
    if event == 'call':
        _depth += 1
    module = frame.f_code.co_filename
    width = 30
    _trace(5, '{d:2d}{s} {name:{w}} | {file}:{lnum}'.format(
        d=_depth, w=(width - _depth),
        s=('>' if event == 'call' else '<') + (' ' * _depth),
        name=frame.f_code.co_name,
        file=module[(module.rindex('/') + 1):],
        lnum=frame.f_code.co_firstlineno))
    if event != 'call':
        _depth -= 1


if __name__ == '__main__':
    logging.basicConfig(
        level=5,  # level=logging.DEBUG,
        filename='/tmp/miur.log',
        datefmt='%H:%M:%S',
        format=("%(asctime)s %(name)8s %(levelname)s " +
                "[%(module)s:%(lineno)d]: %(message)s")
    )

    if __debug__:
        logging.addLevelName(5, 'TRACE')
        _trace = logging.getLogger('trace').log
        _depth = 0
        sys.setprofile(tracefunc)

    M.run(sys.argv)
