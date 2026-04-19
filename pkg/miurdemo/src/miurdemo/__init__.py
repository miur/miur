"""Demo entry points and sample scenarios for miur."""

from miurapp import AppSession
from miurapp import run
from miurcore import Entity


def run_demo() -> str:
    """Produce a small integrated demo string through the app layer."""
    return run(AppSession(focus=Entity(system="demo", handle="root")))
