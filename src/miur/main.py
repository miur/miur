import os


class LocalFileSystem:
    SID = 11

    def scandir(self, path: str) -> tuple[os.DirEntry[str], ...]:
        with os.scandir(path) as it:
            return tuple(it)


class CliUI:
    def show(self, v: object) -> None:
        print(v)


def main() -> None:
    # print("[miur]")
    lfs = LocalFileSystem()
    ui = CliUI()
    d = "/data/g/miur_gen/demo"
    pg = lfs.scandir(d)
    ui.show(pg)
