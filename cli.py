import sys
from typing import Any

from just.iji.main import Context


def navi(**_kw: Any) -> Any:
    lines = sys.stdin.readlines()
    return lines


def main(ctx: Context) -> Any:
    """WiP"""
    args = ctx.args
    cmd = args.pop(0) if args else "navi"

    if cmd == "navi":
        parser = __import__("argparse").ArgumentParser()
        # parser.add_argument("dbs", nargs="+")
        parser.add_argument("-o", "--output")
        parser.add_argument("-c", "--completions", action="store_true")
        parser.add_argument("-f", "--overwrite", action="store_true")
        opts = parser.parse_args(args=args)
        kw = {k: v for k, v in vars(opts).items() if k != "dbs"}
        return navi(**kw)
    raise NotImplementedError
