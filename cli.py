from just.use.iji.main import Context

from .miur import miur

def main(ctx: Context) -> None:
    args = ctx.args
    d = args.pop(0) if args else "/etc"
    miur(d)  # OR:(lazy): __import__("importlib").import_module(".miur", package=__package__).miur(d)
