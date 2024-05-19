from argparse import Action, ArgumentParser
from enum import Enum
from typing import TYPE_CHECKING

from . import _app as APP

if TYPE_CHECKING:
    from just.use.iji.main import Context


class SwitchEnum(Enum):
    true = enable = True
    false = disable = False
    auto = tty = None


class SwitchAction(Action):
    def __call__(self, _ap, ns, s, option_string=None):  # type:ignore
        setattr(ns, self.dest, SwitchEnum[s.lower()])


class SigAction(Action):
    def __call__(self, _ap, ns, s, option_string=None):  # type:ignore
        setattr(ns, self.dest, __import__("signal").Signals["SIG" + s])


def cli_spec(parser: ArgumentParser) -> ArgumentParser:
    o = parser.add_argument
    o("cwd", nargs="?", default="/etc")  # [_] FUT:CHG: os.getcwd()
    o("-v", "--version", action="version", version=APP.__version__)
    _sigset = "HUP INT KILL USR1 USR2 TERM CONT STOP WINCH".split()
    o("-s", "--signal", choices=_sigset, type=str.upper, action=SigAction)
    o("-b", "--backend", choices="selectors asyncio ipython".split())
    # fmt:off
    o("-C", "--color", default="true", choices=SwitchEnum.__members__, type=str.lower, action=SwitchAction)
    return parser


def miur_args(args: list[str]) -> None:
    from .miur import miur_opts

    _ap = ArgumentParser(prog=APP.__appname__, description=APP.__doc__)
    return miur_opts(cli_spec(_ap).parse_args(args))


def miur_ctx(ctx: "Context") -> None:
    return miur_args(ctx.args)
