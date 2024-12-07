from typing import Any, Iterable, Protocol, Self, Type, override


class Representable(Protocol):
    @property
    def name(self) -> str: ...

    # MOVE? "Sortable" ?
    def __lt__(self, other: Self) -> bool:
        return self.name < other.name


class Addressable(Protocol):
    @property
    def loci(self) -> str: ...

    # BAD: we can either sort by loci or by name
    # def __lt__(self, other: Self) -> bool:
    #     return self.loci < other.loci


## ARCH:
#  * on ERROR -> return [`ErrorEntry], mixed with regular entries
#    >> if whole list ~can't be read~ -- it will result in empty list with error
#    COS: we may get multiple errors, for e.g. unreadable elements in the list
#  * on empty list -> return [], and interpret it based on `*Entry itself
#    e.g. to make different messages for empty folder and empty file
#  * if entry is atomic -> return "None", and again interpret it based on `*Entry
#    COS: behavior of HaltEntry is NOT inherent and depends on how we decide to interpret it
#    ALT:BET? remove the method itself and use getattr() to verify its presence
# RENAME? .browse()
class Explorable(Protocol):
    def explore(self) -> "Iterable[Golden]": ...


class Atomic(Addressable, Representable, Protocol):
    __slots__ = ()

    # RENAME? "ATOMIC" | "INTERPRETATION NOT ASSIGNED" | "NO INTERPRETATION" (for blob)
    explore: str = "NOT EXPLORABLE (YET)"


g_entries_cls: "list[Type[Golden]]" = []


class Golden(Explorable, Addressable, Representable, Protocol):
    __slots__ = ()
    altname: str | None = None

    ## [_] FUT: track all *instances* (not classes) and do explicit memory-bound gc-collecting
    # def __new__(cls, *_args: Any, **_kwds: Any) -> Self:
    #     # OFF:REF:(no args/kwds): https://mail.python.org/pipermail/python-dev/2008-February/076854.html
    #     # def __new__[**P](cls, *_args: P.args, **_kwds: P.kwargs) -> Self:
    #     # BAD:(false-positive): https://github.com/pylint-dev/pylint/issues/8325
    #     obj = super().__new__(cls)
    #     print(">>>", obj)
    #     g_entries_cls.append(obj)
    #     return obj

    # ALT: recursively inspect Action.__subclasses__()
    #   REF: https://stackoverflow.com/questions/3862310/how-to-find-all-the-subclasses-of-a-class-given-its-name
    #   REF: https://adamj.eu/tech/2024/05/10/python-all-subclasses/
    # ALT: walk through whole runtime code and inspect everything in existence
    @override
    def __init_subclass__(cls, /, altname: str | None = None, **kwargs: Any) -> None:
        super().__init_subclass__(**kwargs)
        g_entries_cls.append(cls)
        if altname:
            # CASE: making complex entries names easier to use/refer from cmdline
            # USAGE: class FSEntry(Golden, altname='fs'): pass
            cls.altname = altname

        ## MAYBE: map all available `EntryInterperter to original `*Entry data
        ## SEE: :/_try/e31_xlr_entity/e46_ent_3interlace.py
        # global: _g_actions: dict[type, list[type]] = {}
        # ta = tuple(signature(cls.__call__).parameters.values())[1].annotation
        # _g_actions.setdefault(ta, []).append(cls)
