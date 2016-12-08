from .. import base, policy

__all__ = ['QuitCmd']


class QuitCmd(base.BaseCommand):
    """Exit from server only when all cmds in queues processed """
    cmd = 'quit-all'
    # THINK: cmd policy must be per-class or per-instance
    policy = policy.IMMEDIATE

    def __init__(self, ctx, *args):
        # EXPL: immediately ignore all incoming cmds when server is quitting
        ctx.srv.conn.ignoreRecvAll()
        self.bus = ctx.bus
        self.loop = ctx.loop

    # TEMP:HACK: reflect 'quit' back to rotate cycle once more until false condition
    def execute(self):
        self.loop.create_task(self._quit())

    # USE run_forever() and schedule do_quit() on 'quit' cmd
    # MOVE: to fully fledged command QuitCmd
    # BUT:WTF if QuitCmd instance will be destroyed before do_quit() called/finish?
    async def _quit(self):
        # NOTE: qin is already exhausted -- shutdown message is always the last
        # one -- and it triggers closing of executor()
        await self.bus.qin.join()

        # BAD! wait qout only after all executors done!
        #   => qin.pop() is immediate, but qout.put() is often delayed until cmd finished
        # TRY: use semaphore with timer ?
        await self.bus.qout.join()

        # THINK: place 'shutdown' into qout and wait again or send 'shutdown' immediately from server ?
        #   => must traverse 'shutdown' through whole system to establish proper shutdown chain
        # WARN! must be the last action !  No more async coro after this !
        self.loop.stop()
