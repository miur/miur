from typing import Any, Protocol, Type, override, runtime_checkable

from .traits import Addressable, Explorable, Representable

g_entries_cls: "list[Type[Golden]]" = []


@runtime_checkable  # TEMP: to focus_on(match-case Golden())
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

    @override
    def __repr__(self) -> str:
        loci = self.loci
        if not loci.endswith(nm := self.name):
            loci = nm + loci
        if loci.startswith("/"):
            return f"`{loci}"
        else:
            return f"{type(self).__name__}({loci})"
