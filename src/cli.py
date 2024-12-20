from argparse import Action, ArgumentParser
from enum import Enum
from typing import TYPE_CHECKING, override

from . import _pkg
from .app import AppOptions, g_app
from .ui.entity_base import g_entries_cls
from .util.logger import LogLevel, log

if TYPE_CHECKING:
    from just.use.iji.main import Context


# FIXME? only allow 3 values to prevent options sprawling ?
class SwitchEnum(Enum):
    y = yes = true = enable = True
    n = no = false = disable = False
    a = auto = tty = default = None


class SwitchAction(Action):
    @override
    def __call__(self, _ap, ns, s, s_opt=None):  # type:ignore[no-untyped-def]
        # ALT: create enum as Enum('SwitchEnum', {"0":True, ...}) to allow Literal keys too
        # if s == "0": s = "no" elif s == "1": s = "yes"
        setattr(ns, self.dest, SwitchEnum[s.lower()].value)


class SigAction(Action):
    @override
    def __call__(self, _ap, ns, s, s_opt=None):  # type:ignore[no-untyped-def]
        if not isinstance(s, str) or not s.isascii():
            raise NotImplementedError()
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


class EntryCvt(Action):
    @override
    def __call__(self, _ap, ns, s, s_opt=None):  # type:ignore[no-untyped-def]
        if s.islower():
            cls = next(
                x
                for x in g_entries_cls
                if s == x.altname or s == x.__name__.removesuffix("Entry").lower()
            )
        else:
            cls = next(x for x in g_entries_cls if s == x.__name__)
        setattr(ns, self.dest, cls)


def cli_spec(parser: ArgumentParser, *, dfl: AppOptions) -> ArgumentParser:
    o = parser.add_argument
    o("xpath", nargs="?")
    o("-v", "--version", action="version", version=_pkg.__version__)
    _sigset = "HUP INT KILL USR1 USR2 TERM CONT STOP WINCH".split()
    o("-s", "--signal", choices=_sigset, action=SigAction)
    o("-a", "--asyncio", dest="bare", default=dfl.bare, action="store_false")
    o("-b", "--bare", default=dfl.bare, action="store_true")
    # WARN:RQ: import/declare all classess -- to register them
    from .ui import entries  # pylint:disable=unused-import

    _entrycls = (
        *(x.altname or x.__name__.removesuffix("Entry").lower() for x in g_entries_cls),
        *(x.__name__ for x in g_entries_cls),
    )
    o("-i", "--stdinfmt", default=None, choices=_entrycls, action=EntryCvt)
    o("-D", "--devinstall", action="store_true")
    o("-K", "--ipykernel", default=False, action="store_true")
    o("-I", "--ipyconsole", default=None, action="store_false")
    o("-X", "--ipyquit", dest="ipyconsole", action="store_true")
    o("--choosedir", help="write cwd on exit")
    o("--logredir", help="redir to fd or path")
    # pylint:disable=line-too-long
    # fmt:off
    o("-k", "--kill", dest="signal", action="store_const", const=__import__("signal").SIGTERM)
    o("-C", "--color", default=SwitchEnum.default.value, choices=SwitchEnum.__members__, action=SwitchAction)
    # fmt:on
    # BET? Find shortest unique prefix for every word in a given list | Set 1 (Using Trie) - GeeksforGeeks ⌇⡦⣻⢯⣷
    #   https://www.geeksforgeeks.org/find-all-shortest-unique-prefixes-to-represent-each-word-in-a-given-list/
    loglevellst = (
        *LogLevel.__members__,
        *(nm[0] for nm in LogLevel.__members__),
        *(str(m.value) for m in LogLevel.__members__.values()),
    )
    o("-L", "--loglevel", default=log.minlevel, choices=loglevellst, action=LogLevelCvt)
    return parser


def miur_argparse(argv: list[str]) -> None:
    # PERF:(imports): abc ast dis collections.abc enum importlib.machinery itertools linecache
    #    os re sys tokenize token types functools builtins keyword operator collections
    from inspect import get_annotations

    # MAYBE:TODO: different actions based on appname=argv[0]

    o = g_app.opts
    _ap = ArgumentParser(prog=_pkg.__appname__, description=_pkg.__doc__)
    _ns = cli_spec(_ap, dfl=o).parse_args(argv[1:])
    anno = get_annotations(type(o))
    loci = o.__class__.__qualname__
    for k, v in _ns.__dict__.items():
        if k not in anno:  # if not hasattr(opts, k):
            raise KeyError(f".{k}.={v} key not found in {loci}")
        if isinstance(anno[k], str):
            ## ALT:FAIL: forward-annotations undefined in runtime, as they are under #"TYPE_CHECKING"
            # _gs = vars(__import__("sys").modules[AppOptions.__module__])
            # anno = get_annotations(type(o), globals=_gs, eval_str=True)
            log.warning(f"skipping type check for option '{k}'")
        elif not isinstance(v, anno[k]):  # if type(v) is not type(getattr(opts, k)):
            raise ValueError(f"{v}!=.{anno[k]}. wrong type_anno for value in {loci}")
        setattr(o, k, v)
    log.minlevel = LogLevel(o.loglevel)

    if o.PROFILE_STARTUP:
        # TODO: disable for integ-tests e.g. "colored output to ttyalt despite stdout redir"
        log.kpi("argparse")

    from .miur import miur_frontend

    return miur_frontend(g_app)


def miur_ctx(ctx: "Context") -> None:
    return miur_argparse(ctx.args)
