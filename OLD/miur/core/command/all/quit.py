#
# SPDX-FileCopyrightText: 2016 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#
from .. import base, policy

__all__ = ['QuitCmd']


class QuitCmd(base.BaseCommand):
    """Exit from server only when all cmds in queues processed """
    cmd = 'quit-all'
    # THINK: cmd policy must be per-class or per-instance
    policy = policy.IMMEDIATE

    def __init__(self, ctx, *args):
        self._quit = ctx.top.quit_soon

    # TEMP:HACK: reflect 'quit' back to rotate cycle once more until false condition
    # NOTE: even if QuitCmd is destroyed, quit_soon coro continues to work
    def execute(self):
        # USE run_forever() and schedule do_quit() on 'quit' cmd
        self._quit()
