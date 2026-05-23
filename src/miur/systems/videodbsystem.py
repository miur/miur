import lmdb

print(lmdb)

# from typing import Iterable, Iterator, NamedTuple, Self


class VideoDB:
    # views: list[FSFile]
    # pro: list[FSFile]
    # @classmethod
    # def make_anew(cls) -> Self:
    #     views = gather_all("@/view")
    #     pro = gather_all("@/pro")
    #     return cls(views=views, pro=pro)

    def __init__(self, dbpath: str) -> None:
        self._env = lmdb.open(
            dbpath,
            map_size=256 * 1024 * 1024,
            subdir=True,
            create=False,
            lock=True,
            mode=0o644,
            max_readers=1,
            max_dbs=10,
        )
        self._dbmeta = self._env.open_db(b"meta")
        # TODO:(integerkey): verify it's the same
        # assert sys.byteorder == "little"
        # assert ctypes.sizeof(ctypes.c_size_t) == 8

        # DUP:(keys): dupsort=True
        # DUP:(vals): integerdup=True (OR: dupfixed=True)
        self._dbviewpaths = self._env.open_db(b"view")
        self._dbpro = self._env.open_db(b"pro")
        # self.sizes_db = self.env.open_db(b"sizes", dupsort=True)
        # self.names_db = self.env.open_db(b"names", dupsort=True)

    # def get(self, key: str) -> CacheEntry | None:
    #     with self._env.begin(db=self._db, write=False) as txn:
    #         v = txn.get(key.encode("utf-8"))
    #         if not v:
    #             return None
    #         d = json.loads(v.decode("utf-8"))
    #         return CacheEntry(**d)
    #
    # def put(self, ent: CacheEntry) -> None:
    #     b = json.dumps(asdict(ent), separators=(",", ":")).encode("utf-8")
    #     with self._env.begin(db=self._db, write=True) as txn:
    #         txn.put(ent.key.encode("utf-8"), b)

    def close(self) -> None:
        self._env.close()
