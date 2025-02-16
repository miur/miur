if globals().get("TYPE_CHECKING"):
    from typing import Callable


class mp_join_children:
    def __enter__(self) -> None:
        import multiprocessing as MP

        # # NOTE:(startup guard):OR: if __name__ == '__main__':
        # if MP.get_start_method(allow_none=True) != "spawn":
        #     # MP.freeze_support()
        #     # ARCH:DECI:WHY:(spawn):
        #     #   - we need a clean pyenv for each frwk/client, w/o prior #miur settings
        #     #   - #miur is expected to use threads for some ops, which make 'fork' faulty
        #     #   - all closable fildes should be closed, so we do proper exec
        #     ## WARN: should be called only once (during startup)
        #     MP.set_start_method("spawn")

    def __exit__(self, _et, _exc, _tb):  # type:ignore[no-untyped-def]
        from ..app import g_app
        from ..util.exchook import log_exc

        ## SUM: join all spawned processes before exiting main process
        #   ALT:BAD:(uncontrollable?): during initial spawn use "atexit.register(Process(...).join)"
        #     ~~ NICE:HACK: atexit will store ref to .join() which has ref to the Process,
        #        so we don't need to store Process in global "g_app.mp_children" to avoid GC
        mps = g_app.mp_children
        for nm in list(mps):
            try:
                p = mps[nm]
                p.join(timeout=5)
                del mps[nm]
            except Exception as exc:
                log_exc(exc)


def _child_exec(tgt: "Callable[[], None|int]", /) -> None:
    ### DISABLED: supposedly not needed when using MP.set_forkserver_preload("spawn")
    ## TEMP:HACK: exit running miur copy
    #   OR: import asyncio; asyncio.get_running_loop().close()
    # g_app.doexit()  # TODO? && wait_until_miur_exit()
    ## ALT:MAYBE: clean start with same .py interpreter/flags
    # cmd = [.util.devenv.get_py_args()[0], mod.__file__, "-c", "tgt()"]
    # os.execv(cmd[0], cmd)

    import multiprocessing as MP
    import sys

    # ALT: ifx = "*"
    p = MP.current_process()
    ifx = f" [{p.name}@{p.pid}]"

    def _child_log_send(s: str) -> None:
        pos = s.find("]")
        if pos != -1:
            ## ALT: prepend each body
            # body = s[pos + 1 :]
            # pos += len(body) - len(body.lstrip())
            pos += 1
        else:
            ## ALT: always append before newline
            pos = len(s.rstrip())

        s = s[:pos] + ifx + s[pos:]

        ## ALT: if "sys.stderr" wasn't redirected by parent
        # import os
        # from ..app import g_app
        # logback = os.fdopen(g_app.io.logfdchild, "w", encoding="utf-8", buffering=1)
        # logback.write(s)
        # logback.flush()
        sys.stderr.write(s)
        # sys.stderr.flush()

    from ..util.logger import log

    log.write = _child_log_send

    # log.warning(f"forked={sys.modules[tgt.__module__].__file__}")
    log.warning(f"forked={tgt.__module__}.{tgt.__name__}()")

    # HACK:FIXED: allow "print()" for 3rd-party code in children processes
    # [_] BUG? it's too late to do this, if 3rd-party code has print() in module scope
    sys.stdout = sys.stderr

    ## FAIL:COS: "Process" has its own try-catch in BaseProcess._bootstrap,
    #   and sidesteps sys.excepthook by doing hard os._exit(1)
    # sys.excepthook = exception_handler
    ## FIXED: send exceptions into parent process stderr-pipe-logsink
    #   DFL: exceptions in multiprocessing children are simply printed to stderr
    #   SRC: /usr/lib/python3.13/multiprocessing/process.py:290: def _bootstrap(...)
    try:
        # raise RuntimeError("test")  # <DEBUG
        sys.exit(tgt())
    # except:  # ALT: no-var
    #     from ..util.exchook import exception_handler
    #     exception_handler(*sys.exc_info())
    except Exception as exc:
        from ..util.exchook import log_exc

        log_exc(exc)
        # HACK: prevent non-redirected printing of exceptions in child
        sys.exit(1)


# WARN: don't use Threads [which living less than MainThread] for UI
#   as Qt GUI-thread should be never destroyed! (even after app.quit())
# NOTE: importing Qt should also be in the new process/thread
#   COS Qt registers MainThread on import
def spawn_py(tgt: "Callable[[], None|int]", /, nm: str) -> None:
    import multiprocessing as MP

    from ..app import g_app
    from ..util.logger import log

    ## SUM: guard sole spawn per name
    # ALT: for p in MP.active_children() if p.name == nm:
    if p := g_app.mp_children.get(nm, None):
        if p.is_alive():
            log.warning(f"{nm}: child pid={p.pid} is already running! ignored")
            return
        # log.error(f"Err: render={nm} GUI thread should be never destroyed!")
        # return
        p.join()  # <EXPL: reap zombies
        del g_app.mp_children[nm]
        p = None

    try:
        p = MP.Process(name=nm, target=_child_exec, args=(tgt,))
        g_app.mp_children[nm] = p
        p.start()
    finally:
        endmsg = "ed" if p else "ing new child process..."
        log.info(f"{nm}: {MP.get_start_method()}{endmsg}")
