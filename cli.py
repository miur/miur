from argparse import ArgumentParser
from enum import Enum
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from just.use.iji.main import Context


def _just(ctx: "Context") -> None:
    main(ctx.args)


class AutoEnable(Enum):
    # pylint:disable=invalid-name
    true = True  # enable
    false = False  # deisable
    auto = None
    # def __str__(self):
    #     return self.value


def cli_spec(parser: ArgumentParser) -> ArgumentParser:
    o = parser.add_argument
    o("cwd", nargs="?", default="/etc")
    sigenum = __import__("signal").Signals
    o("-s", "--signal", type=sigenum, choices=sigenum)
    o("--color", default="true", choices=AutoEnable, type=AutoEnable.__getitem__)
    o("-f", "--overwrite", action="store_true")
    return parser


def main(args: list[str]) -> Any:
    opts = cli_spec(ArgumentParser()).parse_args(args=args)
    # FAIL: package=(__package__ or __name__)
    mod = __import__("importlib").import_module(".miur", package="just.miur")
    return mod.miur(opts)


if __name__ == "__main__":
    main(__import__("sys").argv[1:])
