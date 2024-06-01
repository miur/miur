from argparse import Action, ArgumentParser
from enum import Enum
from typing import TYPE_CHECKING

from . import _pkg

if TYPE_CHECKING:
    from just.use.iji.main import Context


# FIXME? only allow 3 values to prevent options sprawling ?
class SwitchEnum(Enum):
    y = yes = true = enable = True
    n = no = false = disable = False
    a = auto = tty = default = None


class SwitchAction(Action):
    def __call__(self, _ap, ns, s: str, option_string=None):  # type:ignore
        # ALT: create enum as Enum('SwitchEnum', {"0":True, ...}) to allow Literal keys too
        # if s == "0": s = "no" elif s == "1": s = "yes"
        setattr(ns, self.dest, SwitchEnum[s.lower()].value)


class SigAction(Action):
    def __call__(self, _ap, ns, s, option_string=None):  # type:ignore[no-untyped-def]
        if not isinstance(s, str) or not s.isascii():
            raise NotADirectoryError
        if s.isdigit():
            sig = int(s)
        else:
            sig = __import__("signal").Signals["SIG" + s.upper()].value
        setattr(ns, self.dest, sig)


def cli_spec(parser: ArgumentParser) -> ArgumentParser:
    o = parser.add_argument
    o("cwd", nargs="?", default="/etc")  # [_] FUT:CHG: os.getcwd()
    o("-v", "--version", action="version", version=_pkg.__version__)
    _sigset = "HUP INT KILL USR1 USR2 TERM CONT STOP WINCH".split()
    o("-s", "--signal", choices=_sigset, action=SigAction)
    # BAD: "default" duplicates default value of `AppOptions
    o("-a", "--asyncio", dest="bare", default=True, action="store_false")
    o("-K", "--ipykernel", default=False, action="store_true")
    o("-I", "--ipyconsole", default=False, action="store_true")
    # pylint:disable=line-too-long
    # fmt:off
    o("-k", "--kill", dest="signal", action="store_const", const=__import__("signal").SIGTERM)
    o("-C", "--color", default=SwitchEnum.default.value, choices=SwitchEnum.__members__, action=SwitchAction)
    return parser


def miur_argparse(argv: list[str]) -> None:
    # PERF:(imports): abc ast dis collections.abc enum importlib.machinery itertools linecache
    #    os re sys tokenize token types functools builtins keyword operator collections
    from inspect import get_annotations

    from .app import g
    from .miur import miur_frontend

    # MAYBE:TODO: different actions based on appname=argv[0]

    _ap = ArgumentParser(prog=_pkg.__appname__, description=_pkg.__doc__)
    _ns = cli_spec(_ap).parse_args(argv[1:])
    anno = get_annotations(type(g.opts))
    for k, v in _ns.__dict__.items():
        if k not in anno:  # if not hasattr(opts, k):
            raise KeyError((k, v))
        if not isinstance(v, anno[k]):  # if type(v) is not type(getattr(opts, k)):
            raise ValueError((k, v))
        setattr(g.opts, k, v)
    return miur_frontend(g)


def miur_ctx(ctx: "Context") -> None:
    return miur_argparse(ctx.args)
