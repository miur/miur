if globals().get("TYPE_CHECKING"):
    from typing import Callable


class mp_join_children:
    def __enter__(self) -> None:
        import multiprocessing as MP

        # # NOTE:(startup guard):OR: if __name__ == '__main__':
        if MP.get_start_method(allow_none=True) != "spawn":
            # MP.freeze_support()
            # ARCH:DECI:WHY:(spawn):
            #   - we need a clean pyenv for each frwk/client, w/o prior #miur settings
            #   - #miur is expected to use threads for some ops, which make 'fork' faulty
            #   - all closable fildes should be closed, so we do proper exec
            ## WARN: should be called only once (during startup)
            MP.set_start_method("spawn")

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
    logwrite = sys.stderr.write

    # BET?MOVE:(robust): insert child pid inside parent process when it receives the msg
    #   BAD: impossible to know PID of who wrote to the pipe just now
    #     WKRND:BAD:PERF: https://unix.stackexchange.com/questions/619150/get-pid-of-sending-pipe-process
    #       $ find /proc -lname 'pipe:\[20043922]' 2>/dev/null
    #       ALSO: https://www.thecodingforums.com/threads/attach-to-process-by-pid.744820/
    #   ALT? open individual FD per each child to definitely know
    #     NICE: we will be able to annotate by child PID any arbitrary stdout/stderr writes too!
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
        ## NICE:HACK: reuse reopened "stderr" inof g_app.io.logfdchild "pipe",
        ##   as for MP."spawn" g_app was never initialized and doesn't know FD
        # DECI:MAYBE: make "stderr" into DFL for my logger
        #   + conceptually logs in stderr are fine
        #   + no need to reinit .write here
        #   + auto-redir logs from children
        #   + easier to redir logs together with all apps stderr on cmdline
        #     (especially if I will reframe all writes to original stderr)
        logwrite(s)
        # sys.stderr.flush()

    from ..util.logger import log

    # NOTE: children should re-init log instance
    # log.config(write=_child_log_send, termcolor=True)
    log.write = _child_log_send
    log.termcolor = True

    # log.warning(f"forked={sys.modules[tgt.__module__].__file__}")
    log.warning(f"forked={tgt.__module__}.{tgt.__name__}()")

    # HACK:FIXED: allow "print()" for 3rd-party code in children processes
    if sys.stdout is None:
        import os

        stdout_fd = 1
        os.dup2(sys.stderr.fileno(), stdout_fd, inheritable=True)
        os.set_blocking(stdout_fd, False)
        sys.stdout = os.fdopen(stdout_fd, "w", encoding="utf-8", buffering=1)
        sys.stdout.reconfigure(write_through=True)

    # print("hi", flush=True)  # <DEBUG

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
    import os.path as fs
    import sys

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

    # FIXED~BAD: ModuleNotFoundError: No module named "src"
    entry = getattr(sys.modules["__main__"], "__file__", "")
    pjroot = fs.dirname(fs.dirname(fs.realpath(entry)))
    sys.path.insert(0, pjroot)
    log.error(sys.path)
    try:
        p = MP.Process(name=nm, target=_child_exec, args=(tgt,))
        g_app.mp_children[nm] = p
        p.start()
    finally:
        endmsg = "ed" if p else "ing new child process..."
        log.info(f"{nm}: {MP.get_start_method()}{endmsg}")
        del sys.path[0]
