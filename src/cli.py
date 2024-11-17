from argparse import Action, ArgumentParser
from enum import Enum
from typing import TYPE_CHECKING

from . import _pkg
from .util.logger import LogLevel, log

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


class LogLevelCvt(Action):
    def __call__(self, _ap, ns, s: str, option_string=None):  # type:ignore
        s = s.upper()
        if s.isdigit():
            m = LogLevel(int(s))
        elif len(s) == 1:
            ss = [nm for nm in LogLevel.__members__ if nm[0] == s]
            assert len(ss) == 1
            m = LogLevel[ss[0]]
        else:
            m = LogLevel[s]
        setattr(ns, self.dest, m)


def cli_spec(parser: ArgumentParser) -> ArgumentParser:
    o = parser.add_argument
    o("cwd", nargs="?", default="/etc")  # [_] FUT:CHG: os.getcwd()
    o("-v", "--version", action="version", version=_pkg.__version__)
    _sigset = "HUP INT KILL USR1 USR2 TERM CONT STOP WINCH".split()
    o("-s", "--signal", choices=_sigset, action=SigAction)
    # BAD: "default" duplicates default value of `AppOptions
    o("-a", "--asyncio", dest="bare", default=True, action="store_false")
    o("-D", "--devinstall", action="store_true")
    o("-K", "--ipykernel", default=False, action="store_true")
    o("-I", "--ipyconsole", default=None, action="store_false")
    o("-X", "--ipyquit", dest="ipyconsole", action="store_true")
    o("--logredir", help="redir to fd or path")
    # pylint:disable=line-too-long
    # fmt:off
    o("-k", "--kill", dest="signal", action="store_const", const=__import__("signal").SIGTERM)
    o("-C", "--color", default=SwitchEnum.default.value, choices=SwitchEnum.__members__, action=SwitchAction)
    # BET? Find shortest unique prefix for every word in a given list | Set 1 (Using Trie) - GeeksforGeeks ⌇⡦⣻⢯⣷
    #   https://www.geeksforgeeks.org/find-all-shortest-unique-prefixes-to-represent-each-word-in-a-given-list/
    loglevellst = (*LogLevel.__members__, *(nm[0] for nm in LogLevel.__members__), *(str(m.value) for m in LogLevel.__members__.values()))
    o("-L", "--loglevel", default=log.minlevel, choices=loglevellst, action=LogLevelCvt)
    return parser


def miur_argparse(argv: list[str], devroot: str | None = None) -> None:
    # PERF:(imports): abc ast dis collections.abc enum importlib.machinery itertools linecache
    #    os re sys tokenize token types functools builtins keyword operator collections
    from inspect import get_annotations

    from .app import g_app as g
    from .miur import miur_frontend

    g.opts.devroot = devroot

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
    log.minlevel = g.opts.loglevel
    return miur_frontend(g)


def miur_ctx(ctx: "Context") -> None:
    return miur_argparse(ctx.args)
