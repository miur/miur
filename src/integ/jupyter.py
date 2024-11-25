def ipykernel_start() -> None:
    from ..app import g_app
    from ..util.jupyter import inject_ipykernel_into_asyncio

    loop = __import__("asyncio").get_running_loop()
    # pylint:disable=protected-access
    myns = {"g": g_app, "stdscr": g_app.stdscr, "_main": g_app._main}
    inject_ipykernel_into_asyncio(loop, myns)


def redirect_logs() -> None:
    from ..app import g_app
    from ..iomgr import init_explicit_io

    g_app.opts.logredir = "/t/miur.log"
    init_explicit_io(g_app)


def ipyconsole_out() -> None:
    from ..app import g_app
    from ..loop_asyncio import asyncio_primary_out

    async def _ipy_async() -> None:
        from ..curses_ext import curses_altscreen
        from ..util.jupyter import ipyconsole_async

        with curses_altscreen(g_app.stdscr):
            await ipyconsole_async()

    asyncio_primary_out(g_app, _ipy_async())
