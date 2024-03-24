
# Ma -> f(a -> b) -> Mb

class M<T>:
  privat T value;
  apply(fn = lambda x: M(x + 2))
  {
    return M(fn(value))
  }


f(Ma)
Ma.apply(f).apply(g).apply(h).apply(i)....
Ma(f)(g)(h)(i)....
cat | grep | ...


class Widget:
    def __init__(ent: DirEntity) -> None:
        self._ent = ent


class DirEntity:
    def __init__(path: Path) -> None:
        self._path = path

    def __call__(fnM: Action) -> DirListing:
        return fnM(self._path)


class DirListing:
    def __init__(path: Path) -> None:
        self._path = path

    def __call__(fnM: Action) -> DirListing:
        return fnM(self._path)


class DirListingAction:
    def __call__(path: Path) -> DirListing:
        return DirListing([DirEntity(x) for x in os.lisdir(path)])


DirEntity("/") -> [DirNameAction, DirListingAction, DirStatAction, ...] ->
    DirListingAction -> AsyncCachedListing -> CachedXfm -> View -> [DirEntity("/etc"), ...]
    DirStatAction -> [StatEntity("mtime"), ...]


def main():
    wdg = Widget(DirEntity("/"))
