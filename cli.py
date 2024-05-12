from argparse import Action, ArgumentParser
from enum import Enum
from typing import TYPE_CHECKING

# ALT:HACK: import __main__ as M  && use M.__appname__
#   REF: https://docs.python.org/3/library/__main__.html
__appname__ = "miur"
__version__ = "42.1.3"

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
    o("cwd", nargs="?", default="/etc")  # FUT:CHG: os.getcwd()
    o("-v", "--version", action='version', version=__version__)  # '%(prog)s '+__version__
    _sigset = "HUP INT KILL USR1 USR2 TERM CONT STOP WINCH".split()
    o("-s", "--signal", choices=_sigset, type=str.upper, action=SigAction)
    # fmt:off
    o("-C", "--color", default="true", choices=SwitchEnum.__members__, type=str.lower, action=SwitchAction)
    return parser


def miur_args(args: list[str]) -> None:
    from .miur import miur_opts

    _ap = ArgumentParser(prog=__appname__, description=__doc__)
    return miur_opts(cli_spec(_ap).parse_args(args))


def miur_ctx(ctx: "Context") -> None:
    return miur_args(ctx.args)
