import os
import signal

from .. import PKGSRC_DIR, log


def enable_jurigged() -> None:
    ## BAD: jurigged produces multiple logs per single saved file --> screen refreshes N-times
    ##   TODO: spawn/reset 500ms timer after each event, and only refresh when timer is done

    ## FAIL: when curses is disabled -- no one injects KEY_RESIZE event into getch() to redraw loop
    ##   and we can't even raise exception from here, risking to break python code
    # signal.signal(signal.SIGWINCH, lambda si, fr: ui.refresh())

    import jurigged  # pyright: ignore[reportMissingTypeStubs]

    def jurigged_on_event(event: object) -> None:

        log.verbose(str(event).replace(PKGSRC_DIR, "./"))
        # OR: if "Evaluating" in str(event) or "Update" in str(event): pass; else: return
        if str(event).startswith("Update "):
            ## DISABLED:FAIL: doesn't unblock already waiting getch()
            # try:
            #     # HACK: unblock current .getch()
            #     # C.ungetch(C.KEY_REFRESH)
            #     # C.ungetch(C.KEY_RESIZE)  # safe
            #     # OR:(^L=12): ungetch(12) | ungetch(C.KEY_F5)
            # except C.error:
            #     pass
            ## FIXED:WKRND: Send a native signal to wake up the main thread's getch()
            os.kill(os.getpid(), signal.SIGWINCH)

    ### HACK: hot-reload (recursive ./*.py files from PWD?)
    ## CHECK: if it has import-hook to discover lazily loaded modules later
    # OR: jurigged.watch(pattern=[fs.dirname(fs.realpath(__file__)) + "/**/*.py"], logger=jurigged_on_event)
    jurigged.watch(logger=jurigged_on_event)  # pyright: ignore[reportUnknownMemberType]  # <CASE: recursive
    # jurigged.watch("miur") # <OR watch a specific package directory (non-recursive)
    # import mymod; jurigged.watch(mymod) # <OR watch a specific imported module package

    ### FAIL: how to make !jurigged only reload on demand ?
    # from jurigged.register import registry
    # while True: ... if registry.has_pending(): registry.apply_pending()
